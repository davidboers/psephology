{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Psephology.ElectoralSystems.Rated
    ( -- * Approval voting
      approvalVoting
    , approvalVotes

      -- * Highest median
    , highestMedian

      -- * Score voting
    , scoreVoting

      -- * STAR voting
    , starVoting
    , starFinalists
    )
where

import Data.List (sortOn)

import Psephology.Candidate
import Psephology.ElectoralSystems.Plurality
import Psephology.Counting (scores)
import Psephology.Utils (medianI, tallyWinner)
import Psephology.Voter

-- Approval voting

-- | Returns the index of the [approval voting](https://en.wikipedia.org/wiki/Approval_voting) winner.
approvalVoting :: Voter a => [Candidate] -> [a] -> Int
approvalVoting candidates voters =
    tallyWinner $ approvalVotes candidates voters

-- | Reduces the voter's scoring system to a 0/1 binary, assuming they vote for all candidates with an above average score.
approvalVotes :: Voter a => [Candidate] -> [a] -> [Int]
approvalVotes =
    scores 0 1

-- Highest median

-- | Returns the index of the [highest median voting](https://en.wikipedia.org/wiki/Highest_median_voting_rules) winner.
-- Test suite defaults to @highestMedian 0 10@
highestMedian :: Voter a => Int -> Int -> [Candidate] -> [a] -> Int
highestMedian mn mx candidates voters =
    tallyWinner $ map (\c -> medianI $ map (\v -> score mn mx candidates v c) voters) candidates

-- Score voting

-- | @'scoreVoting' mn mx candidates voters@ returns the index of the [score voting](https://en.wikipedia.org/wiki/Score_voting) winner.
-- Test suite defaults to @scoreVoting 0 10@
scoreVoting :: Voter a => Int -> Int -> [Candidate] -> [a] -> Int
scoreVoting mn mx candidates voters =
    tallyWinner $ scores mn mx candidates voters

-- Star voting

-- | @'starVoting' mn mx candidates voters@ returns the index of the [STAR voting](https://en.wikipedia.org/wiki/STAR_voting) winner.
-- Test suite defaults to @starVoting 0 10@.
starVoting :: Voter a => Int -> Int -> [Candidate] -> [a] -> Int
starVoting mn mx candidates voters =
    let tally = scores mn mx candidates voters
        (f1, f2) = starFinalists tally
     in firstPastThePost [candidates !! f1, candidates !! f2] voters

-- | Returns a tuple of the indexes of the candidates that progress into the second round of the STAR voting election.
starFinalists :: [Int] -> (Int, Int)
starFinalists tally =
    let tally_sorted = sortOn (negate . snd) $ zip [0 .. length tally - 1] tally
        [f1, f2] = take 2 $ map fst tally_sorted
     in (f1, f2)