module Psephology.Counting (votes, antiVotes) where

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