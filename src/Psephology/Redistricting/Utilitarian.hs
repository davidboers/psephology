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
--
-- The distance function generally uses 2D geographic data to analyze the precincts. Additional parameters may be imposed by adding third
-- dimensions. This allows for factors such as race to be taken into account when comparing precincts. In order to ensure geographic contiguity
-- is respected, the distance function reflects [Pareto dominance](https://en.wikipedia.org/wiki/Pareto_efficiency), rather than Euclidean
-- distance. The function for n-dimensional points is below, 2D points still use Euclidean distance.
--
-- \[ d(A,B) = \max\{\Delta_x,\Delta_y\} + \lambda(\Delta_x + \Delta_y) + \sum_{i=3}^{n}\varepsilon\Delta_i \]
--
-- where \(\Delta_k=|A_k - B_k|\), between points \(A,B\) on \(n\) dimensions.
--
-- \[ \lambda = \frac{1}{2B+1} \]
-- \[ \varepsilon = \frac{1}{(2B+1)(B_z+1)} \]
--
-- \(B\) is the bounds for \(\Delta_x,\Delta_y\) and \(B_z=1\) for third dimensions representing proportions, which is the recommended method for
-- handling racial data.
module Psephology.Redistricting.Utilitarian
    ( -- * Precincts
      Precinct (..)
    , precinctsToDistricts
    , utilityP
    , utilityPD
    , utilityDP
    , utilityPDNorm
    , netUtility

      -- * Districts
    , District (..)
    , districtID
    , precinctsD
    , isEstablished
    , isDissolved
    , populationD
    , surplus
    , centerD
    , noNonDissolved
    , utilityD
    , districtsCSV

      -- * District status
    , Status (..)

      -- * Algorithm entry points
    , reduce
    , reduceVerbose

      -- * Algorithm steps
    , distributeSurpluses
    , distributeSurplus
    , mergeSmallest
    , splitSmallest

      -- * Equalizer
    , equalize
    , equalizeVerbose
    , thenEqualizeVerbose
    , redistributeNNNUPrecincts
    ) where

import Data.List
import Data.List.Extras (argmax, argmin)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, isJust)
import Prelude hiding (pi)

import Psephology.Efficiency (distance, phi)
import Psephology.Quotas (hare)

tvp :: [District] -> Int
tvp = sum . map populationD

-- Utility

distance' :: [Double] -> [Double] -> Double
distance' (x1 : y1 : zs1) (x2 : y2 : zs2)
    | null zs1 || null zs2 = distance [x1, y1] [x2, y2]
    | otherwise = primary + secondary + epsilonTerm
    where
        deltaX = abs (x2 - x1)
        deltaY = abs (y2 - y1)
        deltaZs = map abs $ zipWith (-) zs2 zs1
        primary = max deltaX deltaY
        secondary = lambda * (deltaX + deltaY)
        lambda = 1 / (2 * b + 1)
        epsilon = 1 / ((2 * b + 1) * (bz + 1))
        b = 1
        bz = 1
        epsilonTerm = sum $ map (* epsilon) deltaZs
distance' lhs rhs = distance lhs rhs

utility :: [Double] -> [Double] -> Double
utility p1 p2 =
    phi p1 $ distance' p1 p2

-- Precincts

-- | A precinct is a group of voters. It is the smallest indivisible unit. @Precinct@s do not have to be literal voting precincts, and can be
-- counties or even states or provinces if these cannot be divided.
data Precinct = Precinct
    { nameP :: String
    , population :: Int
    , pointZ :: [Double]
    -- ^ Point in a Euclidean space. Ideally the geographic center of a precinct.
    }
    deriving (Eq)

point :: Precinct -> [Double]
point = pointZ

-- | Converts a list of precincts to a list of districts, each containing one precinct. Used at the start of the algorithm.
precinctsToDistricts :: [Precinct] -> [District]
precinctsToDistricts precincts =
    [ District idD [precincts !! (idD - 1)] Continuing (point $ precincts !! (idD - 1))
    | idD <- [1 .. length precincts]
    ]

-- | @'utilityP' lhs rhs@ returns the utility between @lhs@ and @rhs@, multiplied by the population of both precincts.
utilityP :: Precinct -> Precinct -> Double
utilityP lhs rhs =
    fromIntegral (population lhs + population rhs)
        * utility (point lhs) (point rhs)

-- | @'utilityPD' precinct district@ returns the utility between @precinct@ and the center of @district@. Differs from 'utilityDP' in that
-- \(phi\) reflects relativity to the precinct, rather than the district center.
utilityPD :: Precinct -> District -> Double
utilityPD precinct district =
    utility (point precinct) (centerD district)

-- | @'utilityDP' district precinct@ returns the utility between the center of @district@ and @precinct@. Differs from 'utilityPD' in that
-- \(phi\) reflects relativity to the district center, rather than the precinct.
utilityDP :: District -> Precinct -> Double
utilityDP district precinct =
    utility (centerD district) (point precinct)

-- | @'utilityPDNorm' precinct district@ returns the normalized version of 'utilityPD', which reflects a proportion of the maximum utility
-- that can be obtained by @precinct@.
utilityPDNorm :: Precinct -> District -> Double
utilityPDNorm precinct district =
    utilityPD precinct district / distance (replicate (length (point precinct)) 0) (point precinct)

-- | @'netUtility' districts precinct district@ returns the net increase in normalized utility obtained by @precinct@ being allocated to
-- @district@, in comparison to the member of @districts@ that is the highest opportunity cost. The full formula is:
--
-- \[ U_{np}(d) = \frac{\phi_p\circ\|d-p\|}{\|p\|} - \max_{d_i \in D, d_i \neq d} \frac{\phi_p\circ\|d_i-p\|}{\|p\|} \]
netUtility :: [District] -> Precinct -> District -> Double
netUtility districts precinct district =
    let otherDistricts = filter (\district' -> districtID district /= districtID district') districts
     in utilityPDNorm precinct district - maximum (map (utilityPDNorm precinct) otherDistricts)

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
        [Double]
        -- ^ Cached center point, recalculated only when precincts change.

-- | District id.
districtID :: District -> Int
districtID (District idD _ _ _) = idD

-- | List of precincts in the district.
precinctsD :: District -> [Precinct]
precinctsD (District _ precincts _ _) = precincts

-- | The district's 'Status'.
statusD :: District -> Status
statusD (District _ _ status _) = status

-- | @'centerD' district@ returns the median point of @district@, weighed by population.
centerD :: District -> [Double]
centerD (District _ _ _ cachedCenter) =
    cachedCenter

-- | Construct a new District, recalculating center.
mkDistrict :: Int -> [Precinct] -> Status -> District
mkDistrict idD precincts status =
    District idD precincts status (calcCenter precincts)
    where
        calcCenter ps =
            map weightedMedian $ transpose $ map (\p -> map (population p,) $ point p) ps

-- | Update precincts in a District, recalculating center.
updatePrecincts :: District -> [Precinct] -> District
updatePrecincts (District idD _ status _) precincts =
    mkDistrict idD precincts status

-- | Simple way to check whether a district has been established.
isEstablished :: District -> Bool
isEstablished (District _ _ Established _) = True
isEstablished _ = False

-- | Simple way to check whether a district has been dissolved.
isDissolved :: District -> Bool
isDissolved (District _ _ Dissolved _) = True
isDissolved _ = False

-- | Sum of the population in all of the district's precincts.
populationD :: District -> Int
populationD = sum . map population . precinctsD

-- | Returns true if the two district instances are composed of the same precincts.
isSamePrecincts :: District -> District -> Bool
isSamePrecincts (District _ lhs _ _) (District _ rhs _ _) = lhs == rhs

-- | @'surplus' quota district@ is the amount by which @district@'s population exceeds @quota@.
surplus :: Int -> District -> Int
surplus quota district = populationD district - quota

weightedMedian :: (Real w, Ord a) => [(w, a)] -> a
weightedMedian xs =
    let ys = sortOn snd xs -- sort by value
        total = sum (map (realToFrac . fst) ys) :: Double
        half = total / 2
        go _ [] = error "weightedMedian: empty list"
        go acc ((w, v) : rest) =
            let acc' = acc + realToFrac w
             in if acc' >= half then v else go acc' rest
     in go 0 ys

-- | The number of non-dissolved districts.
noNonDissolved :: [District] -> Int
noNonDissolved =
    length . filter (not . isDissolved)

-- | The utility between two districts, based on their center points.
utilityD :: District -> District -> Double
utilityD xi x =
    utility (centerD xi) (centerD x)

-- | Update status in a District, preserving center.
updateStatus :: District -> Status -> District
updateStatus (District idD precincts _ cachedCenter) status =
    District idD precincts status cachedCenter

-- | Returns a list of districts mapped to precincts in CSV format.
districtsCSV :: [Precinct] -> [District] -> String
districtsCSV precincts districts =
    unlines $
        header : map precinctLine precincts
  where
    header = intercalate "," ["Precinct", "District", "Up", "Upn", "Upnn"]
    precinctLine precinct =
        let district = find (\d -> nameP precinct `elem` map nameP (precinctsD d)) districts
        in intercalate ","
            [ nameP precinct
            , maybe "none" (show . districtID) district
            , maybe "0" (show . flip utilityDP precinct) district
            , maybe "0" (show . utilityPDNorm precinct) district
            , maybe "0" (show . netUtility districts precinct) district
            ]

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
        establishRest (d@(District _ _ Continuing _) : ds) = updateStatus d Established : establishRest ds
        establishRest (d : ds) = d : establishRest ds

-- Algorithm steps

reduceStep :: Int -> [District] -> [District]
reduceStep x districts
    | total_surplus_of_established > 0 =
        updateStatuses $ splitSmallest $ distributeSurpluses quota districts
    | otherwise = updateStatuses $ splitSmallest districts
    where
        quota = hare (tvp districts) x
        total_surplus_of_established = sum $ map (surplus quota) $ filter isEstablished districts

        updateStatuses :: [District] -> [District]
        updateStatuses = map updateStatus'

        updateStatus' :: District -> District
        updateStatus' district
            | statusD district == Established = district
            | populationD district >= quota = updateStatus district Established
            | null (precinctsD district) = updateStatus district Dissolved
            | otherwise = district

-- | To csv. @'recordStep' phase n quota lastIter districts@
recordStep :: Phase -> Int -> Int -> [District] -> [District] -> [[String]]
recordStep phase n quota lastIter districts =
    let lastMap = M.fromList [(districtID district, district) | district <- lastIter]
     in map
            ( \district ->
                case M.lookup (districtID district) lastMap of
                    Just lastDistrict@(District _ _ lastStatus _) ->
                        [ show phase
                        , show n
                        , show $ districtID district
                        , show $ noNonDissolved districts
                        , show $ show $ centerD district
                        , show $ length $ precinctsD district
                        , show $ populationD district
                        , show $ populationD district - populationD lastDistrict -- Population change
                        , show quota
                        , show $ surplus quota district
                        , show $ statusD district
                        , show $ lastStatus /= statusD district
                        , show $ currentRatio districts
                        ]
                    Nothing ->
                        [ show phase
                        , show n
                        , show $ districtID district
                        , show $ noNonDissolved districts
                        , show $ show $ centerD district
                        , show $ length $ precinctsD district
                        , show $ populationD district
                        , "N/A" -- Could not find lastIterD
                        , show quota
                        , show $ surplus quota district
                        , show $ statusD district
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
distributeSurplus quota districts district@(District idD precincts _ _)
    | surplusSize > 0 =
        let precinctsSorted =
                sortOn
                    ( \precinct ->
                        netUtility districts precinct district
                    )
                    precincts
            precinctsToTransfer = sort $ selectPrecinctsToTransfer surplusSize precinctsSorted
         in distributeSurplusWorker idD districts precinctsToTransfer
    | otherwise = districts
    where
        surplusSize = surplus quota district

distributeSurplusWorker :: Int -> [District] -> [Precinct] -> [District]
distributeSurplusWorker _ districts [] = districts
distributeSurplusWorker idD districts (x : xs) =
    let id' = transferTo (Just idD) districts x
     in distributeSurplusWorker idD (transfer districts id' x) xs

-- | @'mergeSmallest' districts@ dissolves the non-established member of @districts@ with the smallest
-- population and merges it with the other member of @district@ that provides the most utility for a
-- voter at it's central point.
mergeSmallest :: [District] -> [District]
mergeSmallest districts =
    let smallest@(District _ precincts _ _) = findSmallest districts
        (District mergeInto _ _ _) =
            argmax
                (utilityD smallest)
                ( deleteBy
                    (\(District lhs _ _ _) (District rhs _ _ _) -> lhs == rhs)
                    smallest
                    districts
                )
     in foldl' (`transfer` mergeInto) districts precincts

-- | @'splitSmallest' districts@ dissolves the non-established member of @districts@ with the smallest
-- population, and re-assigns its precincts to the district that maximizes utility.
splitSmallest :: [District] -> [District]
splitSmallest districts =
    let smallest@(District idSmallest precincts _ _) = findSmallest districts
     in foldl'
            ( \districts' precinct ->
                let idD = transferTo (Just idSmallest) districts' precinct
                 in transfer districts' idD precinct
            )
            districts
            (sortOn (`utilityPD` smallest) precincts)

findSmallest :: [District] -> District
findSmallest districts =
    argmin populationD $ filter (not . isEstablished) districts

-- Transfers

selectPrecinctsToTransfer :: Int -> [Precinct] -> [Precinct]
selectPrecinctsToTransfer _ [] = [] -- fallback
selectPrecinctsToTransfer 0 _ = []
selectPrecinctsToTransfer surplusSize (x : xs)
    | surplusWithout > 0 = x : selectPrecinctsToTransfer surplusWithout xs
    | surplusSize > abs surplusWithout = [x]
    | otherwise = []
    where
        surplusWithout = surplusSize - population x

-- | @'transferTo' idD districts precinct@ determines which member of @districts@ provides maximum utility for @precinct@, other than @idD@.
transferTo :: Maybe Int -> [District] -> Precinct -> Int
transferTo idD districts precinct =
    districtID $ argmax (utilityPD precinct) considering
    where
        considering
            | isJust idD = filter ((/=) (fromJust idD) . districtID) districts
            | otherwise = districts

-- | @'transfer' districts idD precinct@ transfers @precinct@ into @idD@ and out of the member of @districts@ it is currently in.
transfer :: [District] -> Int -> Precinct -> [District]
transfer [] _ _ = []
transfer (d@(District id' precincts' _ _) : ds) idD precinct
    | id' == idD =
        updatePrecincts d (precinct : precincts') : transfer ds idD precinct
    | precinct `elem` precincts' =
        updatePrecincts d (delete precinct precincts') : transfer ds idD precinct
    | otherwise = d : transfer ds idD precinct

-- | @'transferToBest' districts precinct@ transfers @precinct@ to whichever member of @districts@ produces the highest utility.
transferToBest :: [District] -> Precinct -> [District]
transferToBest districts precinct =
    transfer districts (transferTo Nothing districts precinct) precinct

-- | Redistributes any precincts with net negative utility to the districts they would prefer to be in.
redistributeNNNUPrecincts :: [District] -> [District]
redistributeNNNUPrecincts districts =
    let nNNUs = concatMap (\dx -> filter (\px -> netUtility districts px dx < 0) $ precinctsD dx) districts
     in foldl transferToBest districts nNNUs

-- Equalizer

-- | @'equalize' districts@ returns @districts@ with populations brought within the bounds of @maxToleranceRatio@. The ratio is equal to the maximum district population divided
-- by the minimum district population. If the ratio cannot be achieved, will stop working at @maxIter@. @maxToleranceRatio@ must be at least 1.
equalize :: [District] -> [District]
equalize districts = snd $ equalizeVerboseWorker [] 0 districts

-- | Analogous to 'reduceVerbose'.
equalizeVerbose :: [District] -> ([[String]], [District])
equalizeVerbose districts =
    equalizeVerboseWorker
        []
        1
        (filter (not . isDissolved) districts)

equalizeVerboseWorker
    :: [[String]] -> Int -> [District] -> ([[String]], [District])
equalizeVerboseWorker record n districts =
    let quota = hare (tvp districts) (length districts)
        thisStep = equalizePairs districts
        record' = record ++ recordStep Equalization n quota districts thisStep
     in if all (uncurry isSamePrecincts) $ zip thisStep districts
            then (record, districts)
            else equalizeVerboseWorker record' (n + 1) thisStep

equalizePairs :: [District] -> [District]
equalizePairs districts =
    let established = [i | i <- [0 .. length districts - 1], isEstablished (districts !! i)]
        pairs = sortOn (\(i, j) -> negate $ abs $ populationD (districts !! i) - populationD (districts !! j)) $ [(i, j) | i <- established, j <- established, i /= j]
     in foldl' equalizePair districts pairs

equalizePair :: [District] -> (Int, Int) -> [District]
equalizePair districts (i, j)
    | populationD di < populationD dj = equalizePair districts (j, i)
    -- \| null precincts =
    | otherwise = distributeSurplusWorker (districtID di) districts precincts
    where
        di = districts !! i
        dj = districts !! j
        relativeSurplus = populationD di - hare (tvp [di, dj]) 2
        precincts = selectPrecinctsToTransfer relativeSurplus $ sortOn (\px -> utilityPD px di - utilityPD px dj) (precinctsD di)

-- | thenEqualizeVerbose $ reduceVerbose noDistricts districts
thenEqualizeVerbose :: ([[String]], [District]) -> ([[String]], [District])
thenEqualizeVerbose (reductionRecord, districts) =
    let (equalizationRecord, equalizedDistricts) = equalizeVerbose districts
     in ( reductionRecord ++ equalizationRecord
        , equalizedDistricts
        )

-- Print to journal and expose
currentRatio :: [District] -> Double
currentRatio districts =
    let nonDissolved = filter (not . isDissolved) districts
     in fromIntegral (maximum $ map populationD nonDissolved)
            / fromIntegral (minimum $ map populationD nonDissolved)
