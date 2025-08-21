module Psephology.Redistricting.Utilitarian
    ( -- * Precincts
      Precinct (..)
    , precinctsToDistricts
    , utilityP
    , utilityPD

      -- * Districts
    , District (..)
    , isEstablished
    , isDissolved
    , populationD
    , surplus
    , centerD
    , noNonDissolved
    , utilityD

      -- * District status
    , Status (..)

      -- * Algorithm entry points
    , reduce
    , reduceVerbose

      -- * Algorithm steps
    , distributeSurpluses
    , distributeSurplus
    )
where

import Data.List (delete, deleteBy, find, foldl', sortOn, transpose)
import Data.List.Extras (argmax, argmin)

import Psephology.Efficiency (utilityV)
import Psephology.Quotas (hare)

-- Precincts

data Precinct = Precinct
    { population :: Int
    , point :: [Double]
    }
    deriving (Eq)

precinctsToDistricts :: [Precinct] -> [District]
precinctsToDistricts precincts =
    [ District idD [precincts !! (idD - 1)] Continuing
    | idD <- [1 .. length precincts]
    ]

-- | @'utilityP' lhs rhs@ returns the utility between @lhs@ and @rhs@, multiplied by the population of both precincts.
utilityP :: Precinct -> Precinct -> Double
utilityP lhs rhs =
    fromIntegral (population lhs + population rhs)
        * utilityV (point lhs) (point rhs)

-- | @'utilityPD' precinct district@ returns the utility between @precinct@ and the center of @district@.
utilityPD :: Precinct -> District -> Double
utilityPD precinct district =
    -- Multiply by population of precinct? should't matter as far as transferTo goes.
    utilityV (point precinct) (centerD district)

-- Districts

data District = District Int [Precinct] Status

-- | Simple way to check whether a district has been established.
isEstablished :: District -> Bool
isEstablished (District _ _ Established) = True
isEstablished _ = False

-- | Simple way to check whether a district has been dissolved.
isDissolved :: District -> Bool
isDissolved (District _ _ Dissolved) = True
isDissolved _ = False

-- | Sum of the population in all of the district's precincts.
populationD :: District -> Int
populationD (District _ precincts _) =
    sum $ map population precincts

-- | @'surplus' quota district@ is the amount by which @district@'s population exceeds @quota@.
surplus :: Int -> District -> Int
surplus quota district = populationD district - quota

-- | Returns the average of a list, or 0.0 for an empty list.
average :: [Double] -> Double
average [] = 0.0
average l  = sum l / fromIntegral (length l)

-- | @'center' district@ returns the center point of @district@.
centerD :: District -> [Double]
centerD (District _ precincts _) =
    map average $ transpose $ map point precincts

-- | The number of non-dissolved districts.
noNonDissolved :: [District] -> Int
noNonDissolved =
    length . filter (not . isDissolved)

utilityD :: District -> District -> Double
utilityD xi x =
    utilityV (centerD xi) (centerD x)

-- Statuses

data Status
    = Continuing
    | Dissolved
    | Established
    deriving (Show, Eq)

-- Algorithm entry points

reduce :: Int -> [District] -> [District]
reduce x districts
    | noNonDissolved districts <= x = districts
    | otherwise = reduce x $ filter (not . isDissolved) $ reduceStep x districts

-- | Verbose reduction with configurable maxIterations (Nothing for unlimited)
reduceVerbose :: Maybe Int -> Int -> [District] -> ([[String]], [District])
reduceVerbose maxIterations x districts =
        reduceVerboseWorker maxIterations quota record 1 x districts
    where
        tvp = sum $ map populationD districts
        quota = hare tvp x
        header =
                [ "n"
                , "ID"
                , "No. districts"
                , "Center"
                , "No. precincts"
                , "Population"
                , "Pop. +/-"
                , "Quota"
                , "Surplus"
                , "Status"
                , "Changed"
                ]
        record = header : recordStep 0 quota districts districts

reduceVerboseWorker
    :: Maybe Int -> Int -> [[String]] -> Int -> Int -> [District] -> ([[String]], [District])
reduceVerboseWorker maxIterations quota record n x districts
    | maybe False (n >) maxIterations = (record, districts)
    | noNonDissolved districts <= x = (record, districts)
    | otherwise =
        reduceVerboseWorker
            maxIterations
            quota
            (record ++ recordStep n quota districts thisStep)
            (n + 1)
            x
            thisStep
  where
    thisStep = reduceStep x $ filter (not . isDissolved) districts

-- Algorithm steps

reduceStep :: Int -> [District] -> [District]
reduceStep x districts
    | total_surplus_of_established > 0 =
        updateStatuses $ distributeSurpluses quota districts
    | otherwise = updateStatuses $ mergeSmallest districts
  where
    tvp = sum $ map populationD districts
    quota = hare tvp x
    total_surplus_of_established = sum $ map (surplus quota) $ filter isEstablished districts

    updateStatuses :: [District] -> [District]
    updateStatuses = map updateStatus

    updateStatus :: District -> District
    updateStatus district@(District idD precincts status)
        | status == Established = district
        | populationD district >= quota = District idD precincts Established
        | null precincts = District idD precincts Dissolved
        | otherwise = district

-- | to csv
recordStep :: Int -> Int -> [District] -> [District] -> [[String]]
recordStep n quota lastIter districts =
    map
        ( \district@(District idD precincts status) ->
            case find (\(District id' _ _) -> id' == idD) lastIter of
                Just (District _ _ lastStatus) ->
                    [ show n
                    , show idD
                    , show $ noNonDissolved districts
                    , show $ show $ centerD district
                    , show $ length precincts
                    , show $ populationD district
                    , show $ populationD district - populationD district
                    , show quota
                    , show $ surplus quota district
                    , show status
                    , show $ lastStatus /= status
                    ]
                Nothing ->
                    [ show n
                    , show idD
                    , show $ noNonDissolved districts
                    , show $ show $ centerD district
                    , show $ length precincts
                    , show $ populationD district
                    , "N/A" -- Could not find lastIterD
                    , show quota
                    , show $ surplus quota district
                    , show status
                    , "N/A" -- Could not compare status
                    ]
        )
        districts

distributeSurpluses :: Int -> [District] -> [District]
distributeSurpluses quota districts =
    foldl
        (distributeSurplus quota)
        districts
        (map (\(District idD _ _) -> idD) $ filter isEstablished districts)

-- | @'distributeSurplus' quota districts idD@ distributes the surplus of @idD@, given @quota@. @districts@ contains all districts, not just established ones.
distributeSurplus :: Int -> [District] -> Int -> [District]
distributeSurplus quota districts idD =
    case find (\(District id' _ _) -> id' == idD) districts of
        Just district@(District _ precincts Established) ->
            let surplusSize = surplus quota district
                precinctsSorted = sortOn (`utilityPD` district) precincts
                precinctsToTransfer = selectPrecinctsToTransfer surplusSize precinctsSorted
             in distributeSurplusWorker idD districts precinctsToTransfer
        _ -> districts -- Fallback

distributeSurplusWorker :: Int -> [District] -> [Precinct] -> [District]
distributeSurplusWorker _ districts [] = districts
distributeSurplusWorker idD districts (x : xs) =
    let id' = transferTo idD districts x
     in distributeSurplusWorker idD (transfer districts id' x) xs

{- | @'mergeSmallest' districts@ dissolves the non-established member of @districts@ with the smallest population and merges it with the other member of @district@ that provides the most utility for a
voter at it's central point.
-}
mergeSmallest :: [District] -> [District]
mergeSmallest districts =
    let smallest@(District _ precincts _) = argmin populationD $ filter (not . isEstablished) districts
        (District mergeInto _ _) =
            argmax
                (utilityD smallest)
                ( deleteBy
                    (\(District lhs _ _) (District rhs _ _) -> lhs == rhs)
                    smallest
                    districts
                )
     in foldl' (`transfer` mergeInto) districts precincts

-- Transfers

selectPrecinctsToTransfer :: Int -> [Precinct] -> [Precinct]
selectPrecinctsToTransfer _ [] = [] -- fallback
selectPrecinctsToTransfer surplusSize (x : xs)
    | surplusWithout > 0 = x : selectPrecinctsToTransfer surplusWithout xs
    | surplusSize > abs surplusWithout = [x]
    | otherwise = []
  where
    surplusWithout = surplusSize - population x

-- | @'transferTo' idD districts precinct@ determines which member of @districts@ provides maximum utility for @precinct@, other than @idD@.
transferTo :: Int -> [District] -> Precinct -> Int
transferTo idD districts precinct =
    (\(District id' _ _) -> id') $
        argmax (utilityPD precinct) $
            filter (\(District id' _ _) -> id' /= idD) districts

-- | @'transfer' districts idD precinct@  transfers @precinct@ into @idD@ and out of the member of @districts@ it is currently in.
transfer :: [District] -> Int -> Precinct -> [District]
transfer [] _ _ = []
transfer (d@(District id' precincts' status') : ds) idD precinct
    | id' == idD =
        District id' (precinct : precincts') status' : transfer ds idD precinct
    | precinct `elem` precincts' =
        District id' (delete precinct precincts') status' : transfer ds idD precinct
    | otherwise = d : transfer ds idD precinct