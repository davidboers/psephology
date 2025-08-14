module Psephology.SampleElections (tennesseeCapital, tennesseeCapitalCandidates) where

import Psephology.Candidate

-- Sample elections

tennesseeCapital :: [[Candidate]]
tennesseeCapital =
    replicate 42 (map Categorical ["Memphis", "Nashville", "Chattanooga", "Knoxville"])
        ++ replicate 26 (map Categorical ["Nashville", "Chattanooga", "Knoxville", "Memphis"])
        ++ replicate 15 (map Categorical ["Chattanooga", "Knoxville", "Nashville", "Memphis"])
        ++ replicate 17 (map Categorical ["Knoxville", "Chattanooga", "Nashville", "Memphis"])

tennesseeCapitalCandidates :: [Candidate]
tennesseeCapitalCandidates = map Categorical ["Memphis", "Nashville", "Chattanooga", "Knoxville"]