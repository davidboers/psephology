-- | See [here](https://en.wikipedia.org/wiki/First-past-the-post_voting) for a detailed explanation.
module Psephology.ElectoralSystems.Plurality (firstPastThePost, antiPlurality) where

import Psephology.Candidate
import Psephology.Counting
import Psephology.Voter
import Psephology.Utils (tallyWinner, tallyLoser)

-- | Returns the index of the candidate that wins a FPTP election.
firstPastThePost :: (Voter a) => [Candidate] -> [a] -> Int
firstPastThePost candidates voters =
    tallyWinner $ votes candidates voters

-- | Returns the index of the candidate that is disliked most by the most voters.
antiPlurality :: (Voter a) => [Candidate] -> [a] -> Int
antiPlurality candidates voters =
    tallyLoser $ antiVotes candidates voters