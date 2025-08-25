-- | The Ancient Greek solution to democracy.
module Psephology.ElectoralSystems.Sortition (sortition, randomBallot) where

import Psephology.Candidate
import Psephology.Voter

import System.Random (Random (randomR), RandomGen, randomR)

-- | @'sortition' g candidates voters@ picks a random @candidate@ and disregards @voters@ entirely.
sortition :: (Voter a, RandomGen g) => g -> [Candidate] -> [a] -> Int
sortition g candidates _
    | null candidates = -1
    | otherwise       = fst (randomR (0, length candidates - 1) g)

-- | @'randomBallot' g candidates voters@ picks a random @voter@ and picks their first choice.
randomBallot :: (Voter a, RandomGen g) => g -> [Candidate] -> [a] -> Int
randomBallot g candidates voters
    | null voters     = -1
    | null candidates = -1
    | otherwise =
        let (i, _) = randomR (0, length voters - 1) g
            vi     = voters !! i
        in preference candidates vi