module Psephology.ElectoralSystem (
    votes,
    antiVotes,
    ElectoralSystem,
    firstPastThePost,
    antiPlurality,
    twoRoundThreshold,
    twoRound,
    firstRoundDecisive,
    instantRunoffVoting,
    traditionalBordaWeight,
    dowdallWeight,
    bordaCount,
    bordaTally,
    dowdallSystem,
    systems,
) where

import Data.List (sortBy)
import Data.List.Extras (argmax, argmin)
import Data.Ord (Down (Down), comparing)

import Psephology.Candidate
import Psephology.Voter

votesOrdinal :: (Voter a) => ([Candidate] -> a -> Int) -> [Candidate] -> [a] -> [Int]
votesOrdinal f candidates voters =
    [ length $ filter (\v -> f candidates v == c) voters
    | c <- [0 .. (length candidates - 1)]
    ]
-- | Returns the tally for each candidate based on 'preference'.
votes :: (Voter a) => [Candidate] -> [a] -> [Int]
votes =
    votesOrdinal preference

-- | Returns the tally for each candidate based on 'lastPreference'.
antiVotes :: (Voter a) => [Candidate] -> [a] -> [Int]
antiVotes =
    votesOrdinal lastPreference

-- First-past-the-post
-- See [here](https://en.wikipedia.org/wiki/First-past-the-post_voting) for a detailed explanation.

-- | Returns the index of the candidate that wins a FPTP election.
firstPastThePost :: (Voter a) => [Candidate] -> [a] -> Int
firstPastThePost candidates voters =
    let tally = votes candidates voters
     in argmax (tally !!) [0 .. length candidates - 1]

-- | Returns the index of the candidate that is disliked most by the most voters.
antiPlurality :: (Voter a) => [Candidate] -> [a] -> Int
antiPlurality candidates voters =
    let tally = antiVotes candidates voters
     in argmin (tally !!) [0 .. length candidates - 1]

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

-- Borda count

-- | @'traditionalBordaWeight' n r@ returns @n@ - @r@ where @n@ is the total number of candidates and @r@ is the candidate rank. If @r@ is -1, returns 0.
traditionalBordaWeight :: Int -> Int -> Double
traditionalBordaWeight _ (-1) = 0
traditionalBordaWeight n r =
    fromIntegral $ n - r

-- | @'dowdallWeight' n r@ returns 1 / @r@ where @n@ is the total number of candidates and @r@ is the candidate rank. If @r@ is -1, returns 0.
dowdallWeight :: Int -> Int -> Double
dowdallWeight _ (-1) = 0
dowdallWeight _ r =
    1 / fromIntegral r

-- | @'bordaTally' weightFormula candidates voters c@ returns the Borda tally for a specific candidate @c@.
bordaTally :: (Voter a) => (Int -> Int -> Double) -> [Candidate] -> [a] -> Candidate -> Double
bordaTally weightFormula candidates voters c =
    sum $ map (weightFormula (length candidates) . rank candidates c) voters

-- | @'bordaCountWFormula' weightFormula candidates voters@ returns the index of the candidate that wins a Borda count using @weightFormula@.
bordaCountWFormula :: (Voter a) => (Int -> Int -> Double) -> [Candidate] -> [a] -> Int
bordaCountWFormula weightFormula candidates voters =
    argmax (bordaTally weightFormula candidates voters . (candidates !!)) [0 .. length candidates - 1]

-- | Shortcut for 'bordaCountWFormula'
bordaCount :: (Voter a) => [Candidate] -> [a] -> Int
bordaCount = bordaCountWFormula traditionalBordaWeight

-- | Shortcut for 'bordaCountWFormula'
dowdallSystem :: (Voter a) => [Candidate] -> [a] -> Int
dowdallSystem = bordaCountWFormula dowdallWeight

type ElectoralSystem a = [Candidate] -> [a] -> Int

systems :: (Voter a) => [(String, ElectoralSystem a)]
systems =
    [ ("FPTP", firstPastThePost)
    , ("Anti-plurality", antiPlurality)
    , ("TRS", twoRound)
    , ("IRV", instantRunoffVoting)
    , ("Borda", bordaCount)
    , ("Dowdall", dowdallSystem)
    ]
