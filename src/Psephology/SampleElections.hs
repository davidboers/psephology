-- | Contains some example elections and referenda for experimentation.
module Psephology.SampleElections
    ( -- * Condorcet cycle
      condorcetCycle
    , condorcetCycleCandidates

      -- * Proportional representation
    , proportionalRepresentation1

      -- * Referendum
    , smallReferendum
    , yesOrNo

      -- * Tennessee capital
    , tennesseeCapital
    , tennesseeCapitalCandidates

      -- * Score
    , approval
    , approvalCandidates
    ) where

import Psephology.Candidate

-- Tennessee capital

-- | The Tennessee example is borrowed from [Wikipedia](https://en.wikipedia.org/wiki/Template:Tenn_voting_example),
--     where it is used in several articles to explain voting systems. It contains 100 voters and 4 candidate cities
--     (Memphis, Nashville, Chattanooga, and Knoxville). The example is ideal because it has a Condorcet winner, follows
--     single-peaked preferences, and different voting systems provide different results.
--
--     @
--         let candidates = 'tennesseeCapitalCandidates'
--         let voters = 'tennesseeCapital'
--         'firstPastThePost' candidates voters    = 0      -- Memphis
--         'instantRunoffVoting' candidates voters = 3      -- Knoxville
--         'condorcetWinner' candidates voters     = Just 1 -- Nashville
--     @
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

-- | Categorical Candidate instances of the four cities.
tennesseeCapitalCandidates :: [Candidate]
tennesseeCapitalCandidates = map Categorical ["Memphis", "Nashville", "Chattanooga", "Knoxville"]

-- Condorcet cycle

-- | The Condorcet cycle example involves 3 candidates (A, B, C) and 20 voters. It is a minimal example of a Condorcet cycle.
--
--     5 voters: \(A \succ B \succ C\)
--
--     8 voters: \(B \succ C \succ A\)
--
--     7 voters: \(C \succ A \succ B\)
condorcetCycle :: [[Candidate]]
condorcetCycle =
    replicate 5 (map Categorical ["A", "B", "C"])
        ++ replicate 8 (map Categorical ["B", "C", "A"])
        ++ replicate 7 (map Categorical ["C", "A", "B"])

-- | Categorical Candidate instances of the three candidates.
condorcetCycleCandidates :: [Candidate]
condorcetCycleCandidates = map Categorical ["A", "B", "C"]

-- Referendum

-- | 10 voters, 2 options (Yes/No)
smallReferendum :: [[Candidate]]
smallReferendum =
    replicate 6 (map Categorical ["Yes", "No"])
        ++ replicate 4 (map Categorical ["No", "Yes"])

yesOrNo :: [Candidate]
yesOrNo = map Categorical ["Yes", "No"]

-- Proportional representation

proportionalRepresentation1 :: [Int]
proportionalRepresentation1 =
    [ 2000 -- A
    , 1900 -- B
    , 900 -- C
    , 850 -- D
    , 575 -- E
    , 450 -- F
    , 400 -- G
    ]

-- Thiele

approval :: [[[Candidate]]]
approval =
    replicate
        3
        [ [Categorical "A", Categorical "B", Categorical "C", Categorical "D"]
        , [Categorical "E"]
        ]
        ++ replicate
            4
            [ [Categorical "A", Categorical "B", Categorical "C"]
            , [Categorical "D", Categorical "E"]
            ]
        ++ replicate
            2
            [ [Categorical "A", Categorical "C"]
            , [Categorical "B", Categorical "D", Categorical "E"]
            ]
        ++ replicate
            2
            [ [Categorical "A", Categorical "B", Categorical "D"]
            , [Categorical "C", Categorical "E"]
            ]
        ++ replicate
            3
            [ [Categorical "A", Categorical "D"]
            , [Categorical "B", Categorical "C", Categorical "E"]
            ]
        ++ replicate
            1
            [ [Categorical "A", Categorical "D", Categorical "E"]
            , [Categorical "B", Categorical "C"]
            ]
        ++ replicate
            3
            [ [Categorical "D", Categorical "E"]
            , [Categorical "A", Categorical "B", Categorical "C"]
            ]
        ++ replicate
            2
            [ [Categorical "E"]
            , [Categorical "A", Categorical "B", Categorical "C", Categorical "D"]
            ]

approvalCandidates :: [Candidate]
approvalCandidates =
    map Categorical ["A", "B", "C", "D", "E"]