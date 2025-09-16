module Psephology.ElectoralSystems.Runoff (
    twoRoundThreshold,
    twoRound,
    firstRoundDecisive,
    instantRunoffVoting,
    coombsMethod,
) where

import Data.List
import Data.Ord (comparing)

import Psephology.Candidate
import Psephology.Counting
import Psephology.ElectoralSystems.Plurality
import Psephology.Voter
import Psephology.Utils (tallyWinner, tallyLoser)

-- Two-round system

-- | Returns the index of the candidate that wins a TRS election.
twoRoundThreshold :: (Voter a) => Double -> [Candidate] -> [a] -> Int
twoRoundThreshold floatingThreshold candidates voters =
    let tally = votes candidates voters
     in if firstRoundDecisive floatingThreshold tally
            then tallyWinner tally
            else secondRound candidates voters tally

-- | Shortcut for 'twoRoundThreshold' with the threshold set at 50%.
twoRound :: (Voter a) => [Candidate] -> [a] -> Int
twoRound = twoRoundThreshold 0.5

secondRound :: (Voter a) => [Candidate] -> [a] -> [Int] -> Int
secondRound candidates voters prevTally =
    let pairIndexes = includeListTRS prevTally
        pair = map (candidates !!) pairIndexes
     in pairIndexes !! firstPastThePost pair voters

firstRoundDecisive :: Double -> [Int] -> Bool
firstRoundDecisive floatingThreshold tally =
    let solidThreshold = floor $ floatingThreshold * fromIntegral (sum tally)
     in maximum tally > solidThreshold

-- | Returns a list of the indexes of the candidates in the top 2 positions.
includeListTRS :: [Int] -> [Int]
includeListTRS tally =
    let cutoff = minimum $ take 2 $ sortBy (comparing negate) tally
     in findIndices (>= cutoff) tally

-- Instant-Runoff Voting

-- | Returns the index of the candidate that wins an IRV election.
instantRunoffVoting :: (Voter a) => [Candidate] -> [a] -> Int
instantRunoffVoting candidates voters =
    instantRunoff includeListIRV candidates voters (votes candidates voters)

-- | Single iteration of IRV. No bulk exclusions or early elections.
instantRunoff :: (Voter a) => ([Candidate] -> [a] -> [Int] -> [Int]) -> [Candidate] -> [a] -> [Int] -> Int
instantRunoff _ [_] _ _ = 0
instantRunoff includeList candidates voters prevTally =
    let newSetIndexes = includeList candidates voters prevTally
        newSet = map (candidates !!) newSetIndexes
     in newSetIndexes !! instantRunoff includeList newSet voters (votes newSet voters)

includeListIRV :: (Voter a) => [Candidate] -> [a] -> [Int] -> [Int]
includeListIRV candidates _ tally =
    let excluding = tallyLoser tally
     in filter (excluding /=) [0 .. (length candidates - 1)]

-- [Coombs' method](https://en.wikipedia.org/wiki/Coombs%27_method)

coombsMethod :: (Voter a) => [Candidate] -> [a] -> Int
coombsMethod candidates voters =
    instantRunoff coombsInclude candidates voters (votes candidates voters)

coombsInclude :: (Voter a) => [Candidate] -> [a] -> [Int] -> [Int]
coombsInclude candidates voters _ =
    let antitally = antiVotes candidates voters
        excluding = tallyWinner antitally
     in filter (excluding /=) [0 .. (length candidates - 1)]
