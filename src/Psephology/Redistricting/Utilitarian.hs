{-# LANGUAGE TupleSections #-}

-- | Uses a utilitarian algorithm to reapportion election districts. The user inputs a list of precincts and a number of districts (@x@),
-- and the algorithm reduces the precincts to @x@, and ensures they are all of the same population. Below is a basic outline of the procedure:
--
-- * All @precincts@ are placed in separate districts.
-- * Every district, starting with the least populous, is merged with that district that maximizes a utility function.
-- * As districts reach the quota for population size, they are re-arranged so that the excess voters are placed in other districts. This is
-- achieved by removing the precincts that have the lowest utility in another district, which in practice means those on the border
-- of the established district. When districts reach the quota, they are referred to as "established".
-- * Once the number of established districts is reduced to @x@, the algorithm transitions to the "equalization" phase. District surpluses are
-- transferred again to ensure population equality within a given ratio. However, now we take into account population deviations to ensure we
-- don't enlarge districts that already have too many people.
--
-- The algorithm is similar to the counting process of the [single transferable vote](https://en.wikipedia.org/wiki/Single_transferable_vote) (STV).
-- While other redistricting algorithms focus on brute force, this algorithm is much more direct and efficient. Rather than wandering towards
-- the best possible map, it seeks it out via the utility function.
module Psephology.Redistricting.Utilitarian
    ( -- * Precincts
      Precinct (..)
    , precinctsToDistricts
    , utilityP
    , utilityPD
    , utilityDP
    , utilityDPNorm
    , netUtility

      -- * Districts
    , District (..)
    , districtID
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

      -- * Optimizer
    , optimize
    , optimizeVerbose
    , thenOptimizeVerbose

      -- * Equalizer
    , equalize
    , equalizeVerbose
    , thenEqualizeVerbose
    ) where

import Data.List (delete, deleteBy, foldl', sort, sortOn, transpose)
import Data.List.Extras (argmax, argmin)
import qualified Data.Map.Strict as M

import Psephology.Efficiency (utilityV)
import Psephology.Quotas (hare)

tvp :: [District] -> Int
tvp = sum . map populationD

-- Precincts

-- | A precinct is a group of voters. It is the smallest indivisible unit. @Precinct@s do not have to be literal voting precincts, and can be
-- counties or even states or provinces if these cannot be divided.
data Precinct = Precinct
    { nameP :: String
    , population :: Int
    , point :: [Double]
    -- ^ Point in a Euclidean space. Ideally the geographic center of a precinct.
    }
    deriving (Eq)

-- | Converts a list of precincts to a list of districts, each containing one precinct. Used at the start of the algorithm.
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

-- | @'utilityPD' precinct district@ returns the utility between @precinct@ and the center of @district@. Differs from 'utilityDP' in that
-- \(phi\) reflects relativity to the precinct, rather than the district center.
utilityPD :: Precinct -> District -> Double
utilityPD precinct district =
    utilityV (point precinct) (centerD district)

-- | @'utilityDP' district precinct@ returns the utility between the center of @district@ and @precinct@. Differs from 'utilityPD' in that
-- \(phi\) reflects relativity to the district center, rather than the precinct.
utilityDP :: District -> Precinct -> Double
utilityDP district precinct =
    utilityV (centerD district) (point precinct)

-- | @'utilityDPNorm' district precinct@ returns the normalized version of 'utilityDP', which reflects a proportion of the maximum utility
-- that can be obtained by @district@.
utilityDPNorm :: District -> Precinct -> Double
utilityDPNorm district precinct =
    utilityDP district precinct / sqrt (sum $ map (** 2) $ centerD district)

-- | @'netUtility' districts precinct district@ returns the net increase in normalized utility obtained by @precinct@ being allocated to
-- @district@, in comparison to the member of @districts@ that is the highest opportunity cost. The full formula is:
--
-- \[ U_{np}(d) = \frac{\phi_p\circ\|d-p\|}{\|d\|} - \max_{d_i \in D, d_i \neq d} \frac{\phi_p\circ\|d_j-p\|}{\|d_j\|} \]
netUtility :: [District] -> Precinct -> District -> Double
netUtility districts precinct district =
    let otherDistricts = filter (\district' -> districtID district /= districtID district') districts
     in utilityDPNorm district precinct - maximum (map (`utilityDPNorm` precinct) otherDistricts)

instance Ord Precinct where
    compare lhs rhs =
        case compare (head $ point lhs) (head $ point rhs) of
            EQ ->
                compare (Precinct "" 0 (tail $ point lhs)) (Precinct "" 0 (tail $ point rhs))
            a -> a

-- Districts

-- | Represents a district.
data District
    = District
        Int
        -- ^ A numerical value representing the district's id.
        [Precinct]
        -- ^ A non-empty list of precincts included in the district.
        Status
        -- ^ The district's 'Status'.

-- | District id.
districtID :: District -> Int
districtID (District idD _ _) = idD

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

-- | @'centerD' district@ returns the center point of @district@, weighed by population.
centerD :: District -> [Double]
centerD (District _ precincts _) =
    map xbar $ transpose $ map (\p -> map (fromIntegral (population p) *) $ point p) precincts
    where
        xbar :: [Double] -> Double
        xbar xs = sum xs / totalPopulation
        totalPopulation = fromIntegral $ sum (map population precincts)

-- | The number of non-dissolved districts.
noNonDissolved :: [District] -> Int
noNonDissolved =
    length . filter (not . isDissolved)

-- | The utility between two districts, based on their center points.
utilityD :: District -> District -> Double
utilityD xi x =
    utilityV (centerD xi) (centerD x)

-- Statuses

-- | Every district has one of the statuses.
data Status
    = -- | The default.
      Continuing
    | -- | After a district has been excluded and merged due to having too small a population.
      Dissolved
    | -- | A district that has struck the quota (at some point) and now won't be dissolved.
      Established
    deriving (Show, Eq)

-- Algorithm entry points

data Phase
    = Reduction
    | Optimization
    | Equalization
    deriving (Show)

-- | The entry point of the reduction phase of the algorithm. Given @x@ and @districts@, call
-- @reduce x districts@. The method will return a list of districts with length @x@. Further
-- equalization may be necessary. Use 'reduceVerbose' for details of the algorithm's progress.
--
-- +TODO: Shortcut for 'reduceVerbose' that drops the first tuple element.
reduce :: Int -> [District] -> [District]
reduce x districts
    | noNonDissolved districts <= x = districts
    | otherwise = reduce x $ filter (not . isDissolved) $ reduceStep x districts

-- | Returns a tuple @(record, districts')@ given @x@ number of districts and input list
-- @districts@. The @record@ is a list of decisions on each iteration of the reduction phase.
-- Ideal for export to a CSV file.
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
            , "Population ratio"
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
        updateStatuses $ mergeSmallest $ distributeSurpluses quota districts
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

-- | To csv. @'recordStep' phase n quota lastIter districts@
recordStep :: Phase -> Int -> Int -> [District] -> [District] -> [[String]]
recordStep phase n quota lastIter districts =
    let lastMap = M.fromList [(idD, district) | district@(District idD _ _) <- lastIter]
     in map
            ( \district@(District idD precincts status) ->
                case M.lookup idD lastMap of
                    Just lastDistrict@(District _ _ lastStatus) ->
                        [ show phase
                        , show n
                        , show idD
                        , show $ noNonDissolved districts
                        , show $ show $ centerD district
                        , show $ length precincts
                        , show $ populationD district
                        , show $ populationD district - populationD lastDistrict -- Population change
                        , show quota
                        , show $ surplus quota district
                        , show status
                        , show $ lastStatus /= status
                        , show $ currentRatio districts
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
                        , show $ currentRatio districts
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
        let isEqualizing = all isEstablished districts
            precinctsSorted =
                sortOn
                    ( \precinct ->
                        if isEqualizing
                            then netUtility districts precinct district
                            else utilityPD precinct district
                    )
                    precincts
            precinctsToTransfer = sort $ selectPrecinctsToTransfer surplusSize precinctsSorted
         in distributeSurplusWorker quota idD districts precinctsToTransfer
    | otherwise = districts
    where
        surplusSize = surplus quota district

distributeSurplusWorker :: Int -> Int -> [District] -> [Precinct] -> [District]
distributeSurplusWorker _ _ districts [] = districts
distributeSurplusWorker quota idD districts (x : xs) =
    let id' = transferTo quota idD districts x
     in distributeSurplusWorker quota idD (transfer districts id' x) xs

-- | @'mergeSmallest' districts@ dissolves the non-established member of @districts@ with the smallest population and merges it with the other member of @district@ that provides the most utility for a
-- voter at it's central point.
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

-- @'transferTo' quota idD districts precinct@ determines which member of @districts@ provides maximum utility for @precinct@, other than @idD@.
transferTo :: Int -> Int -> [District] -> Precinct -> Int
transferTo quota idD districts precinct
    | null deficitDistricts || all isEstablished districts =
        districtID $ argmax (utilityPD precinct) otherDistricts
    | otherwise =
        districtID $ argmax (utilityPD precinct) deficitDistricts
    where
        otherDistricts = filter (\(District id' _ _) -> id' /= idD) districts
        deficitDistricts = filter (\district' -> surplus quota district' < 0) otherDistricts

-- @'transfer' districts idD precinct@  transfers @precinct@ into @idD@ and out of the member of @districts@ it is currently in.
transfer :: [District] -> Int -> Precinct -> [District]
transfer [] _ _ = []
transfer (d@(District id' precincts' status') : ds) idD precinct
    | id' == idD =
        District id' (precinct : precincts') status' : transfer ds idD precinct
    | precinct `elem` precincts' =
        District id' (delete precinct precincts') status' : transfer ds idD precinct
    | otherwise = d : transfer ds idD precinct

-- Optimize

optimize :: [District] -> [District]
optimize = snd . optimizeVerbose

-- | Analogous to 'reduceVerbose'.
optimizeVerbose :: [District] -> ([[String]], [District])
optimizeVerbose districts = optimizeVerboseWorker [] 1 quota districts
    where
        quota = hare (tvp districts) (length districts)

optimizeVerboseWorker :: [[String]] -> Int -> Int -> [District] -> ([[String]], [District])
optimizeVerboseWorker record 50 _ districts = (record, districts)
optimizeVerboseWorker record n quota districts
    | null netNegativePrecincts = (record, districts)
    | otherwise =
        let districts' = foldl' (optimizationStep quota) nonDissolved netNegativePrecincts
            record' = record ++ recordStep Optimization n quota districts districts'
         in optimizeVerboseWorker record' (n + 1) quota districts'
    where
        nonDissolved = filter (not . isDissolved) districts
        netNegativePrecincts =
            concatMap
                ( \district'@(District idD precincts _) ->
                    map (idD,) $
                        filter (\precinct -> netUtility nonDissolved precinct district' < 0) precincts
                )
                nonDissolved

optimizationStep :: Int -> [District] -> (Int, Precinct) -> [District]
optimizationStep quota districts (idD, precinct) =
    let newDistrictID = transferTo quota idD districts precinct
     in transfer districts newDistrictID precinct

-- | thenOptimizeVerbose $ reduceVerbose noDistricts districts
thenOptimizeVerbose :: ([[String]], [District]) -> ([[String]], [District])
thenOptimizeVerbose (record, districts) =
    let (optimizationRecord, optimizedDistricts) = optimizeVerbose districts
     in ( record ++ optimizationRecord
        , optimizedDistricts
        )

-- Equalizer

-- | @'equalize' maxIter maxToleranceRatio districts@ returns @districts@ with populations brought within the bounds of @maxToleranceRatio@. The ratio is equal to the maximum district population divided
-- by the minimum district population. If the ratio cannot be achieved, will stop working at @maxIter@. @maxToleranceRatio@ must be at least 1.
equalize :: Int -> Double -> [District] -> [District]
equalize maxIter maxToleranceRatio districts = snd $ equalizeVerboseWorker [] 0 maxIter maxToleranceRatio districts

-- | Analogous to 'reduceVerbose'.
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
    | currentRatio districts <= maxToleranceRatio = (record, districts)
    | otherwise =
        let quota = hare (tvp districts) (length districts)
            thisStep = distributeSurpluses quota districts
            record' = record ++ recordStep Equalization n quota districts thisStep
         in equalizeVerboseWorker
                record'
                (n + 1)
                maxIter
                maxToleranceRatio
                thisStep

-- | thenEqualizeVerbose maxIter maxToleranceRatio $ reduceVerbose noDistricts districts
thenEqualizeVerbose
    :: Int -> Double -> ([[String]], [District]) -> ([[String]], [District])
thenEqualizeVerbose maxIter maxToleranceRatio (reductionRecord, districts) =
    let (equalizationRecord, equalizedDistricts) = equalizeVerbose maxIter maxToleranceRatio districts
     in ( reductionRecord ++ equalizationRecord
        , equalizedDistricts
        )

-- Print to journal and expose
currentRatio :: [District] -> Double
currentRatio districts =
    let nonDissolved = filter (not . isDissolved) districts
     in fromIntegral (maximum $ map populationD nonDissolved)
            / fromIntegral (minimum $ map populationD nonDissolved)
