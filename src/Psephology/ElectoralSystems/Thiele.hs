-- | [Thiele's voting rules](https://en.wikipedia.org/wiki/Thiele%27s_voting_rules)
module Psephology.ElectoralSystems.Thiele
    ( thiele
    , thieleAddition

      -- * Utility functions
    , indicator
    , harmonic

      -- * Runtime
    , estimateRuntime
    , estimateRuntimeGeneral
    , describeSeconds
    ) where

import Data.List (findIndices, intersect, subsequences)
import Data.List.Extras (argmax, argmaxWithMax)
import Data.Time.Clock
import Data.Fixed

import Psephology.Candidate
import Psephology.Voter
import Psephology.Utils (factorial)

-- | @'thiele' f candidates voters x@ returns a subset of @candidates@ of length @x@ that maximizes
-- a utility function @f@ across all @voters@. Returns an empty list if @x@ is not a positive integer
-- or it exceeds the number of candidates. There are three options for picking a utility function:
--
-- \[
--      f(r) = r\\
--      g(r) = 1 \text{ and } g(0) = 0\\
--      h(r) = \sum_{i=1}^{r} \frac{1}{i}
-- \]
--
-- where \(r\) is the number of approved candidates in the committee for a given voter. In Haskell:
--
-- @
--      thiele 'fromIntegral' -- First option, essentially an identity function.
--      thiele 'indicator'    -- Second option, ensures as many factions as possible are represented.
--      thiele 'harmonic'     -- Third option, guarantees [justified representation](https://en.wikipedia.org/wiki/Justified_representation).
-- @
--
--     +WARNING: This method has [exponential worst-case complexity](https://en.wikipedia.org/wiki/NP-hardness)
-- and may become impractical with more than ~10 candidates. \(\mathcal{O}((n\text{ choose }k)nm)\).
thiele :: Voter a => (Int -> Double) -> [Candidate] -> [a] -> Int -> [Int]
thiele f candidates voters x
    | x < 1 || x > n = []
    | otherwise      = argmax (evaluateCommittee f candidates voters) committees
    where
        n = length candidates
        committees = filter ((x ==) . length) $ subsequences [0 .. n - 1]

evaluateCommittee :: Voter a => (Int -> Double) -> [Candidate] -> [a] -> [Int] -> Double
evaluateCommittee f candidates voters committee =
    let r v =
            let ai = findIndices (\ci -> score 0 1 candidates v ci == 1) candidates
             in length $ intersect ai committee
     in sum $ map (f . r) voters

-- | Heuristic version of 'thiele' that approximates the winning set. Use in case Thiele's full 
-- runtime is excessive. 
--
-- Uses an iterative process, adding the candidate that proves the maximum increase in total 
-- satisfaction.
thieleAddition :: Voter a => (Int -> Double) -> [Candidate] -> [a] -> Int -> [Int]
thieleAddition f candidates voters x
    | x < 1 || x > n = []
    | otherwise      = fst $ iterate (additionRound f candidates voters) ([], 0) !! x
    where
        n = length candidates

additionRound :: Voter a => (Int -> Double) -> [Candidate] -> [a] -> ([Int], Double) -> ([Int], Double)
additionRound f candidates voters (committee, u) =
    argmaxWithMax (\committee' -> evaluateCommittee f candidates voters committee' - u) is
    where
        is = [committee ++ [i] | i <- [0 .. length candidates - 1], i `notElem` committee]

-- Utility functions

-- | Returns @0@ if @r@ is 0 and 1 otherwise.
indicator :: Int -> Double
indicator 0 = 0
indicator _ = 1

-- | Returns the harmonic number of @r@.
harmonic :: Int -> Double
harmonic r =
    sum [1 / fromIntegral n | n <- [1 .. r]]

-- Runtime

-- | @'estimateRuntime' candidates voters x@ returns the predicted number of seconds required to 
-- calculate the winning 'thiele' committee without a heuristic.
estimateRuntime :: Voter a => [Candidate] -> [a] -> Int -> IO Pico
estimateRuntime candidates voters x = do
    let n = length candidates
    if x < 1 || x > n then error "x must be between 0 and the number of candidates"
    else do
        let n' = toInteger n
            x' = toInteger x
            iters = factorial n' `div` (factorial x' * factorial (n' - x'))
        startTime <- getCurrentTime
        let tally = evaluateCommittee harmonic candidates voters (take x [0..n-1])
        tally `seq` return ()
        endTime <- getCurrentTime
        let diff = nominalDiffTimeToSeconds $ diffUTCTime endTime startTime
        return $ diff * fromIntegral iters

-- | @'estimateRuntimeGeneral' n m x@ calls 'estimateRuntime' for number of candidates @n@, 
-- number of voters @m@, and number of seats @x@.
-- 
-- @
--      > 'describeSeconds' <$> estimateRuntimeGeneral 5 1000 3
--      "0.079123000000 seconds"
--      > 'describeSeconds' <$> estimateRuntimeGeneral 20 10000 3
--      "4.376927400000 minutes"
--      > 'describeSeconds' <$> estimateRuntimeGeneral 40 20000 10
--      "33.210087456744 years"
-- @
estimateRuntimeGeneral :: Int -> Int -> Int -> IO Pico
estimateRuntimeGeneral n m x = 
    let candidates = [ Categorical $ show i | i <- [1..n] ]
        voters     = replicate m candidates
     in estimateRuntime candidates voters x

-- | Indicates how long a number of seconds is, in terms of years, days, hours, or minutes, 
-- whichever is greatest.
describeSeconds :: Pico -> String
describeSeconds pico 
    | years   > 1 = show years ++ " years"
    | days    > 1 = show days ++ " days"
    | hours   > 1 = show hours ++ " hours"
    | minutes > 1 = show minutes ++ " minutes"
    | otherwise   = show pico ++ " seconds"
    where
        years   = days / 365
        days    = hours / 24
        hours   = minutes / 60
        minutes = pico / 60