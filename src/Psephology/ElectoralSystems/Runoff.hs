module Psephology.ElectoralSystems.Runoff (
    twoRoundThreshold,
    twoRound,
    firstRoundDecisive,
    instantRunoffVoting,
) where

import Data.List (sortBy)
import Data.List.Extras (argmax, argmin)
import Data.Ord (Down (Down), comparing)

import Psephology.Candidate
import Psephology.Counting
import Psephology.ElectoralSystems.Plurality
import Psephology.Voter

-- Two-round system

-- | Returns the index of the candidate that wins a TRS election.
twoRoundThreshold :: (Voter a) => Double -> [Candidate] -> [a] -> Int
twoRoundThreshold floatingThreshold candidates voters =
    let tally = votes candidates voters
     in if firstRoundDecisive floatingThreshold tally
            then argmax (tally !!) [0 .. (length candidates - 1)]
            else secondRound candidates voters tally

-- | Shortcut for 'twoRoundThreshold' with the threshold set at 50%.
twoRound :: (Voter a) => [Candidate] -> [a] -> Int
twoRound = twoRoundThreshold 0.5

secondRound :: (Voter a) => [Candidate] -> [a] -> [Int] -> Int
secondRound candidates voters prevTally =
    let pairIndexes = includeListTRS candidates prevTally
        pair = map (candidates !!) pairIndexes
     in pairIndexes !! firstPastThePost pair voters

firstRoundDecisive :: Double -> [Int] -> Bool
firstRoundDecisive floatingThreshold tally =
    let solidThreshold = floor $ floatingThreshold * fromIntegral (sum tally)
     in maximum tally > solidThreshold

-- | Returns a list of the indexes of the candidates in the top 2 positions.
includeListTRS :: [Candidate] -> [Int] -> [Int]
includeListTRS candidates tally =
    let cutoff = minimum $ take 2 $ sortBy (comparing Data.Ord.Down) tally
     in [ i
        | i <- [0 .. (length candidates - 1)]
        , (tally !! i) >= cutoff
        ]

-- Instant-Runoff Voting

-- | Returns the index of the candidate that wins an IRV election.
instantRunoffVoting :: (Voter a) => [Candidate] -> [a] -> Int
instantRunoffVoting candidates voters =
    instantRunoff candidates voters (votes candidates voters)

-- | Single iteration of IRV. No bulk exclusions or early elections.
instantRunoff :: (Voter a) => [Candidate] -> [a] -> [Int] -> Int
instantRunoff [_] _ _ = 0
instantRunoff candidates voters prevTally =
    let newSetIndexes = includeListIRV candidates prevTally
        newSet = map (candidates !!) newSetIndexes
     in newSetIndexes !! instantRunoff newSet voters (votes newSet voters)

includeListIRV :: [Candidate] -> [Int] -> [Int]
includeListIRV candidates tally =
    let excluding = argmin (tally !!) [0 .. (length candidates - 1)]
     in filter (excluding /=) [0 .. (length candidates - 1)]