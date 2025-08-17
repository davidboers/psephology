module Psephology.ElectoralSystems.Plurality (firstPastThePost, antiPlurality) where

import Data.List.Extras (argmax, argmin)

import Psephology.Candidate
import Psephology.Counting
import Psephology.Voter

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