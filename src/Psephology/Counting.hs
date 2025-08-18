module Psephology.Counting (votes, antiVotes, scores) where

import Psephology.Candidate
import Psephology.Voter

votesOrdinal :: (Voter a) => ([Candidate] -> a -> Int) -> [Candidate] -> [a] -> [Int]
votesOrdinal f candidates =
    foldl
        ( \t v ->
            [ if f candidates v == i then t !! i + 1 else t !! i
            | i <- [0 .. length candidates - 1]
            ]
        )
        (replicate (length candidates) 0)

-- | Returns the tally for each candidate based on 'preference'.
votes :: (Voter a) => [Candidate] -> [a] -> [Int]
votes =
    votesOrdinal preference

-- | Returns the tally for each candidate based on 'lastPreference'.
antiVotes :: (Voter a) => [Candidate] -> [a] -> [Int]
antiVotes =
    votesOrdinal lastPreference

-- | Returns the total scores for each candidate.
scores :: (Voter a) => Int -> Int -> [Candidate] -> [a] -> [Int]
scores mn mx candidates =
    foldl
        ( \t v ->
            [ t !! i + score mn mx candidates v (candidates !! i)
            | i <- [0 .. length candidates - 1]
            ]
        )
        (replicate (length candidates) 0)
