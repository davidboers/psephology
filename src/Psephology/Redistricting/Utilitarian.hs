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
    , mergeSmallest

      -- * Equalizer
    , equalize
    , equalizeVerbose
    , thenEqualizeVerbose
    ) where

import Data.List (delete, deleteBy, foldl', sortOn, transpose)
import qualified Data.Map.Strict as M
import Data.List.Extras (argmax, argmin)

import Psephology.Efficiency (utilityV)
import Psephology.Quotas (hare)

tvp :: [District] -> Int
tvp = sum . map populationD

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

instance Ord Precinct where
    compare lhs rhs =
        case compare (head $ point lhs) (head $ point rhs) of
            EQ -> compare (Precinct 0 (tail $ point lhs)) (Precinct 0 (tail $ point rhs))
            a -> a

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
average l = sum l / fromIntegral (length l)

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

data Phase
    = Reduction
    | Equalization
    deriving (Show)

reduce :: Int -> [District] -> [District]
reduce x districts
    | noNonDissolved districts <= x = districts
    | otherwise = reduce x $ filter (not . isDissolved) $ reduceStep x districts

reduceVerbose :: Int -> [District] -> ([[String]], [District])
reduceVerbose x districts =
    reduceVerboseWorker quota record 1 x districts
  where
    quota = hare (tvp districts) x
    header =
        [ "Phase"
        , "n"
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
    record = header : recordStep Reduction 0 quota districts districts

reduceVerboseWorker
    :: Int
    -> [[String]]
    -> Int
    -> Int
    -> [District]
    -> ([[String]], [District])
reduceVerboseWorker quota record n x districts
    | noNonDissolved districts <= x = (record, establishRest districts)
    | otherwise =
        reduceVerboseWorker
            quota
            (record ++ recordStep Reduction n quota districts thisStep)
            (n + 1)
            x
            thisStep
  where
    thisStep = reduceStep x $ filter (not . isDissolved) districts

    establishRest :: [District] -> [District]
    establishRest [] = []
    establishRest ((District idD precincts Continuing) : ds) = District idD precincts Established : establishRest ds
    establishRest (d : ds) = d : establishRest ds

-- Algorithm steps

reduceStep :: Int -> [District] -> [District]
reduceStep x districts
    | total_surplus_of_established > 0 =
        updateStatuses $ distributeSurpluses quota districts
    | otherwise = updateStatuses $ mergeSmallest districts
  where
    quota = hare (tvp districts) x
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
recordStep :: Phase -> Int -> Int -> [District] -> [District] -> [[String]]
recordStep phase n quota lastIter districts =
    let lastMap = M.fromList [(idD, district) | district@(District idD _ _) <- lastIter]
    in map
        ( \district@(District idD precincts status) ->
            case M.lookup idD lastMap of
                Just (District _ _ lastStatus) ->
                    [ show phase
                    , show n
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
                    [ show phase
                    , show n
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

-- | @'distributeSurpluses' quota districts@ returns @districts@ with all surpluses redistributed given @quota@.
distributeSurpluses :: Int -> [District] -> [District]
distributeSurpluses quota districts =
    foldl
        (distributeSurplus quota)
        districts
        (filter isEstablished districts)

-- | @'distributeSurplus' quota districts district@ distributes the surplus of @district@, given @quota@. @districts@ contains all districts, not just established ones.
distributeSurplus :: Int -> [District] -> District -> [District]
distributeSurplus quota districts district@(District idD precincts _)
    | surplusSize > 0 =
        let precinctsSorted = sortOn (`utilityPD` district) precincts
            precinctsToTransfer = selectPrecinctsToTransfer surplusSize precinctsSorted
         in distributeSurplusWorker idD districts precinctsToTransfer
    | otherwise = districts
  where
    surplusSize = surplus quota district

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

-- Equalizer

{- | @'equalize' maxIter maxToleranceRatio districts@ returns @districts@ with populations brought within the bounds of @maxToleranceRatio@. The ratio is equal to the maximum district population divided
by the minimum district population. If the ratio cannot be achieved, will stop working at @maxIter@. @maxToleranceRatio@ must be at least 1.
-}
equalize :: Int -> Double -> [District] -> [District]
equalize maxIter maxToleranceRatio districts = snd $ equalizeVerboseWorker [] 0 maxIter maxToleranceRatio districts

equalizeVerbose :: Int -> Double -> [District] -> ([[String]], [District])
equalizeVerbose maxIter maxToleranceRatio districts
    | maxToleranceRatio < 1 = ([], districts)
    | otherwise =
        equalizeVerboseWorker
            []
            1
            maxIter
            maxToleranceRatio
            (filter (not . isDissolved) districts)

equalizeVerboseWorker
    :: [[String]] -> Int -> Int -> Double -> [District] -> ([[String]], [District])
equalizeVerboseWorker record n maxIter maxToleranceRatio districts
    | n > maxIter = (record, districts)
    | currentRatio <= maxToleranceRatio = (record, districts)
    | otherwise =
        let quota = hare (tvp districts) (length districts)
            thisStep = distributeSurpluses quota districts
            newRecord = record ++ recordStep Equalization n quota districts thisStep
         in equalizeVerboseWorker
                newRecord
                (n + 1)
                maxIter
                maxToleranceRatio
                thisStep
  where
    currentRatio =
        fromIntegral (maximum $ map populationD districts)
            / fromIntegral (minimum $ map populationD districts)

-- | thenEqualizeVerbose maxIter maxToleranceRatio $ reduceVerbose noDistricts districts
thenEqualizeVerbose
    :: Int -> Double -> ([[String]], [District]) -> ([[String]], [District])
thenEqualizeVerbose maxIter maxToleranceRatio (reductionRecord, districts) =
    let (equalizationRecord, equalizedDistricts) = equalizeVerbose maxIter maxToleranceRatio districts
     in ( reductionRecord ++ equalizationRecord
        , equalizedDistricts
        )
