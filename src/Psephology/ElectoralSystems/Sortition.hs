-- | The Ancient Greek solution to democracy.
module Psephology.ElectoralSystems.Sortition (sortition, randomBallot) where

import Psephology.Candidate
import Psephology.Voter

import Data.Random
import Data.Function
import System.Random.Stateful

-- | @'sortition' g candidates voters@ picks a random @candidate@ and disregards @voters@ entirely.
sortition :: (Voter a, RandomGen g) => g -> [Candidate] -> [a] -> Int
sortition _ []         _ = -1
sortition g candidates _ = fst (randomR (0, length candidates - 1) g)

-- | @'randomBallot' g candidates voters@ picks a random @voter@ and picks their first choice.
randomBallot :: (Voter a, RandomGen g) => g -> [Candidate] -> [a] -> Int
randomBallot _ []         _      = -1
randomBallot _ _          []     = -1
randomBallot g candidates voters =
    randomR (0, length voters - 1) g
        & fst
        & (voters !!)
        & preference candidates