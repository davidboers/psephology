-- | [Thiele's voting rules](https://en.wikipedia.org/wiki/Thiele%27s_voting_rules)
module Psephology.ElectoralSystems.Thiele
    ( thiele

      -- * Utility functions
    , indicator
    , harmonic
    ) where

import Data.List (findIndices, intersect, subsequences)
import Data.List.Extras (argmax)

import Psephology.Candidate
import Psephology.Voter

-- | @'thiele' f candidates voters x@ returns a subset of @candidates@ of length @x@ that maximizes
-- a utility function @f@ across all @voters@. Returns an empty list if @x@ is not a positive integer
-- or it exceeds the number of candidates. There are three options for picking a utility function:
--
-- \[
--      f(r) = r
--      g(r) = 1 \text{ and } g(0) = 0
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
-- and may become impractical with more than ~10 candidates.
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

-- Utility functions

-- | Returns @0@ if @r@ is 0 and 1 otherwise.
indicator :: Int -> Double
indicator 0 = 0
indicator _ = 1

-- | Returns the harmonic number of @r@.
harmonic :: Int -> Double
harmonic r =
    sum [1 / fromIntegral n | n <- [1 .. r]]