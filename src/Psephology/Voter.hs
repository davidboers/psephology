{-# LANGUAGE FlexibleInstances #-}

module Psephology.Voter (Voter (..), lastPreference, truncateAt, dist) where

import Data.List
import Data.List.Extras.Argmax (argmin)
import Data.Maybe

import Psephology.Candidate

class Voter a where
    -- | @'preference' candidates v@ returns the index of the candidate in @candidates@ that is the voter's highest choice between the given @candidates@.
    preference :: [Candidate] -> a -> Int

    -- | @'rank' v c@ returns voter @v@'s ranking for @c@ or -1 if truncated.
    rank :: [Candidate] -> a -> Candidate -> Int

    -- | @'score' mn mx v c@ returns voter @v@'s score (between @mn@ and @mx@, both inclusive) for @c@.
    score :: Int -> Int -> [Candidate] -> a -> Candidate -> Int

-- | Modeled voter preferences
instance Voter [Double] where
    preference candidates v = argmin (\n -> dist v (candidates !! n)) [0 .. (length candidates - 1)]

    rank candidates v = rank candidates (sortOn (dist v) candidates)

    -- \| Uses a relative formula
    score mn mx candidates v c =
        mn + round (fromIntegral (mx - mn) * (1.0 - ((dc - min_d) / (max_d - min_d))))
      where
        dc = dist v c
        d = map (dist v) candidates
        min_d = minimum d
        max_d = maximum d

-- | Given voter preferences, ex. from a .blt file.
instance Voter [Candidate] where
    preference _ [] = -1
    preference candidates (nextChoice : v) =
        fromMaybe (preference candidates v) $ elemIndex nextChoice candidates

    rank _ v c =
        (+ 1) $ fromMaybe (-2) (elemIndex c v) -- Ew

    -- \| Not recommended, this instance is intended for ordinal preferences. Returns the traditional Borda score of the candidate
    score mn _ candidates v c =
        length candidates - rank candidates v c + mn

lastPreference :: (Voter a) => [Candidate] -> a -> Int
lastPreference [_] _ = 0
lastPreference candidates v =
    let p = preference candidates v
        indexes_without = filter (p /=) [0 .. length candidates - 1]
        candidates_without = map (candidates !!) indexes_without
     in indexes_without !! lastPreference candidates_without v

-- | For limited preferential voting
truncateAt :: Int -> [[Candidate]] -> [[Candidate]]
truncateAt k = map (take k)

-- | Euclidean distance between a spacial voter and a spacial candidate
dist :: [Double] -> Candidate -> Double
dist p1 (NamedSpacial _ p2) = dist p1 (Spacial p2)
dist p1 (Spacial p2) = sqrt $ sum $ dist1D p1 p2
dist _ _ = error "Attempting to calculate distance between spacial and categorical candidates."

dist1D :: [Double] -> [Double] -> [Double]
dist1D (p1i : p1) (p2i : p2) = ((p2i - p1i) ** 2) : dist1D p1 p2
dist1D [] _ = []
dist1D _ [] = []