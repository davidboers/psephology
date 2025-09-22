{-# LANGUAGE FlexibleInstances #-}

-- | Voters come in two types, theoretical and real. This duality is intended to
-- allow the user to experiment with real election results (or mock-ups used to study
-- concepts in real elections) as well as the theoretical implications of voting systems.
--
-- Theoretical voters exist on a Euclidean space. The user may choose any number of
-- dimensions. They are specified by a point on this space, represented by a list of Doubles.
-- There exists an instance of Voter to facilitate the use of these lists.
--
-- Real voters are represented as a list of ordered 'Candidate's.
module Psephology.Voter
    ( Voter (..)
    , lastPreference

      -- * Spacial helpers
    , utilityM

      -- * Formal helpers
    , truncateAt

      -- * Functions that should be moved
    , argmaxC
    , argminC
    ) where

import Data.List
import Data.List.Extras.Argmax (argmax, argmin)
import Data.Maybe

import Psephology.Candidate
import Psephology.Efficiency (utilityV)

-- | Voters must be able to distinguish between candidates in an ordinal manner (relatively better/worse) and
-- in a cardinal manner (generally good or bad). This allows the same voters to participate in both ordinal and
-- cardinal elections. There are three common functions for all Voters:
--
-- * 'preference', which returns the Voter's first choice amongst a list of alternatives.
-- * 'rank', which returns a candidate's relative position within a Voter's preferences.
-- * 'score', which returns the Voter's level of support of a candidate on a given scale.
--
-- There are two native instances of the class. Explanations of the distinguishing features is found in the
-- documentation below. The behavior of the class methods in each instance is:
--
-- +--------------+------------------------------------------------------------------------------------+------------------------------------------------------------------------+
-- | Method       | @[Double]@                                                                         | @['Candidate']@                                                        |
-- +==============+====================================================================================+========================================================================+
-- | 'preference' | The voter's top choice amongst a group of candidates.                                                                                                       |
-- +--------------+------------------------------------------------------------------------------------+------------------------------------------------------------------------+
-- | 'rank'       | One more than the number of candidates with a smaller distance from the voter.     | Self-explanatory.                                                      |
-- +--------------+------------------------------------------------------------------------------------+------------------------------------------------------------------------+
-- | 'score'      | The distance between a candidate and the voter composed on a given relative scale. | Not recommended. Returns the traditional Borda score of the candidate. |
-- +--------------+------------------------------------------------------------------------------------+------------------------------------------------------------------------+
class Voter a where
    -- | @'preference' candidates v@ returns the index of the candidate in @candidates@ that is the
    -- voter's highest choice between the given @candidates@. Returns -1 if no preference is expressed
    -- between @candidates@.
    preference :: [Candidate] -> a -> Int

    -- | @'rank' v c@ returns voter @v@'s ranking for @c@ or -1 if truncated.
    rank :: [Candidate] -> a -> Candidate -> Int

    -- | @'score' mn mx v c@ returns voter @v@'s score (between @mn@ and @mx@, both inclusive) for @c@.
    score :: Int -> Int -> [Candidate] -> a -> Candidate -> Int

-- | Used to represent voters on a Euclidean space (spacial voters). Uses a strict linear order of preferences.
instance Voter [Double] where
    preference candidates v = argmaxC (utilityM v) candidates

    rank candidates v = rank candidates (sortOn (negate . utilityM v) candidates)

    score mn mx candidates v c =
        mn + round (fromIntegral (mx - mn) * ((dc - min_d) / (max_d - min_d)))
        where
            dc = utilityM v c
            d = map (utilityM v) candidates
            min_d = minimum d
            max_d = maximum d

-- | A voter's preferences given as a list of options, ex. from a .blt file. A [total ordering](https://en.wikipedia.org/wiki/Total_order).
instance Voter [Candidate] where
    preference _ [] = -1
    preference candidates (nextChoice : v) =
        fromMaybe (preference candidates v) $ elemIndex nextChoice candidates

    rank _ v c =
        maybe (-1) (+1) $ elemIndex c v

    score mn _ candidates v c =
        length candidates - rank candidates v c + mn

-- | A voter's preferences given as a list of options, ex. from a .blt file. A [weak ordering](https://en.wikipedia.org/wiki/Weak_ordering). Proof of concept.
instance Voter [[Candidate]] where
    preference _ [] = -1
    preference candidates v =
        argminC (rank candidates v) candidates

    rank _ v c =
        maybe (-1) (+1) $ findIndex (elem c) v

    score mn _ candidates v c =
        length v - rank candidates v c + mn

-- | Returns the index of the candidate preferred least amongst the input list.
-- The method does not need to be specialized for different instances of @Voter@,
-- because it works by simply excluding the preferred candidate from the input list
-- until there is only one left.
--
-- Undefined behavior for some truncated ballots.
lastPreference :: Voter a => [Candidate] -> a -> Int
lastPreference [_] _ = 0
lastPreference candidates v
    | p == -1 = 0
    | otherwise = indexes_without !! lastPreference candidates_without v
    where
        p = preference candidates v
        indexes_without = filter (p /=) [0 .. length candidates - 1]
        candidates_without = map (candidates !!) indexes_without

-- | For limited preferential voting. Allows a list of formal preferences amongst
-- real candidates to be cut off after a certain point @k@. This particular method is
-- intended strictly for elections where voters are restricted to a certain number of
-- given rankings (*limited* preferential, not *optional*). Individual ballots can be
-- truncated simply:
--
-- > take 2 [Categorical "A", Categorical "B", Categorical "C"] == [Categorical "A", Categorical "B"]
truncateAt
    :: Int
    -- ^ Number of preferences to count.
    -> [[Candidate]]
    -- ^ List of lists of voters.
    -> [[Candidate]]
truncateAt k = map (take k)

-- | Monadic helper for calculating the utility of a candidate for a spacial voter.
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