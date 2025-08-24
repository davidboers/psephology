module Psephology.SampleElections
    ( tennesseeCapital
    , tennesseeCapitalCandidates
    , condorcetCycle
    , condorcetCycleCandidates
    ) where

import Psephology.Candidate

-- * Tennessee capital

tennesseeCapital :: [[Candidate]]
tennesseeCapital =
    replicate
        42
        (map Categorical ["Memphis", "Nashville", "Chattanooga", "Knoxville"])
        ++ replicate
            26
            (map Categorical ["Nashville", "Chattanooga", "Knoxville", "Memphis"])
        ++ replicate
            15
            (map Categorical ["Chattanooga", "Knoxville", "Nashville", "Memphis"])
        ++ replicate
            17
            (map Categorical ["Knoxville", "Chattanooga", "Nashville", "Memphis"])

tennesseeCapitalCandidates :: [Candidate]
tennesseeCapitalCandidates = map Categorical ["Memphis", "Nashville", "Chattanooga", "Knoxville"]

-- * Condorcet cycle

condorcetCycle :: [[Candidate]]
condorcetCycle =
    replicate 5 (map Categorical ["A", "B", "C"])
        ++ replicate 8 (map Categorical ["B", "C", "A"])
        ++ replicate 7 (map Categorical ["C", "A", "B"])

condorcetCycleCandidates :: [Candidate]
condorcetCycleCandidates = map Categorical ["A", "B", "C"]