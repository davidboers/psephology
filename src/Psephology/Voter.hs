{-# LANGUAGE FlexibleInstances #-}

module Psephology.Voter (Voter (..), lastPreference, truncateAt, utilityM, argmaxC, argminC) where

import Data.List
import Data.List.Extras.Argmax (argmax, argmin)
import Data.Maybe

import Psephology.Candidate
import Psephology.Efficiency (utilityV)

class Voter a where
    -- | @'preference' candidates v@ returns the index of the candidate in @candidates@ that is the voter's highest choice between the given @candidates@.
    preference :: [Candidate] -> a -> Int

    -- | @'rank' v c@ returns voter @v@'s ranking for @c@ or -1 if truncated.
    rank :: [Candidate] -> a -> Candidate -> Int

    -- | @'score' mn mx v c@ returns voter @v@'s score (between @mn@ and @mx@, both inclusive) for @c@.
    score :: Int -> Int -> [Candidate] -> a -> Candidate -> Int

-- | Modeled voter preferences
instance Voter [Double] where
    preference candidates v = argmaxC (utilityM v) candidates

    rank candidates v = rank candidates (sortOn (negate . utilityM v) candidates)

    -- \| Uses a relative formula
    score mn mx candidates v c =
        mn + round (fromIntegral (mx - mn) * ((dc - min_d) / (max_d - min_d)))
      where
        dc = utilityM v c
        d = map (utilityM v) candidates
        min_d = minimum d
        max_d = maximum d

-- | Given voter preferences, ex. from a .blt file.
instance Voter [Candidate] where
    preference _ [] = -1
    preference candidates (nextChoice : v) =
        fromMaybe (preference candidates v) $ elemIndex nextChoice candidates

    rank _ v c =
        case elemIndex c v of
            Just a -> a + 1
            Nothing -> length v + 1

    -- \| Not recommended, this instance is intended for ordinal preferences. Returns the traditional Borda score of the candidate
    score mn _ candidates v c =
        length candidates - rank candidates v c + mn

lastPreference :: Voter a => [Candidate] -> a -> Int
lastPreference [_] _ = 0
lastPreference candidates v
    | p == -1 = 0
    | otherwise = indexes_without !! lastPreference candidates_without v
  where
    p = preference candidates v
    indexes_without = filter (p /=) [0 .. length candidates - 1]
    candidates_without = map (candidates !!) indexes_without

-- | For limited preferential voting
truncateAt :: Int -> [[Candidate]] -> [[Candidate]]
truncateAt k = map (take k)

utilityM :: [Double] -> Candidate -> Double
utilityM p1 (NamedSpacial _ p2) = utilityM p1 (Spacial p2)
utilityM p1 (Spacial p2) = utilityV p1 p2
utilityM _ _ = error "Attempting to calculate utility of a categorical candidate."

-- | @'argmaxC' f candidates@ returns the index of the candidate that maximizes @f@.
argmaxC :: Ord b => (Candidate -> b) -> [Candidate] -> Int
argmaxC f candidates = argmax (\i -> f $ candidates !! i) [0 .. length candidates - 1]

-- | @'argminC' f candidates@ returns the index of the candidate that minimizes @f@.
argminC :: Ord b => (Candidate -> b) -> [Candidate] -> Int
argminC f candidates = argmin (\i -> f $ candidates !! i) [0 .. length candidates - 1]