{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Runners.TAP

import Data.Bifunctor qualified
import Data.Maybe

import Psephology.BLT
import Psephology.Candidate
import Psephology.Condorcet
import Psephology.ElectoralSystem
import Psephology.SampleElections
import Psephology.Spoilers
import Psephology.Voter

useTAP :: Bool
useTAP = False

main :: IO ()
main
    | useTAP = defaultMainWithIngredients [tapRunner] tests
    | otherwise = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "Tests"
        [testTennesseeCapitalElection]

testTennesseeCapitalElection =
    testGroup
        "Tennessee Capital election"
        [ testWinnerBySystem
            candidates
            voters
            [ "Memphis"
            , "Nashville" -- Or Chattanooga
            , "Nashville"
            , "Knoxville"
            , "Nashville"
            , "Nashville"
            , "Nashville"
            ]
        , testCondorcetWinner candidates voters "Nashville"
        , testCase "(pairwise scores)" $
            condorcetMatrix numPreferOver candidates voters
                @?= [ [100, 58, 58, 58]
                    , [42, 100, 32, 32]
                    , [42, 68, 100, 17]
                    , [42, 68, 83, 100]
                    ]
        , testCase "(.blt export)" $
            export candidates voters "Tennessee capital election"
                @?= "4 1\n42 1 2 3 4 0\n26 2 3 4 1 0\n15 3 4 2 1 0\n17 4 3 2 1 0\n0\n\"Memphis\"\n\"Nashville\"\n\"Chattanooga\"\n\"Knoxville\"\n\"Tennessee capital election\"\n"
        , testClones
            candidates
            voters
            [ ["Chattanooga", "Knoxville"]
            , ["Nashville", "Chattanooga", "Knoxville"]
            ]
        , testSpoilers
            candidates
            voters
            [ []
            , ["Memphis"]
            , ["Chattanooga", "Knoxville"]
            , ["Memphis", "Nashville"]
            , []
            , []
            ]
        , testProxies
            candidates
            voters
            [ [("Memphis", "Nashville")]
            , [("Nashville", "Chattanooga")]
            , [("Chattanooga", "Knoxville")]
            , [("Chattanooga", "Knoxville")]
            , [("Nashville", "Chattanooga")]
            , [("Memphis", "Nashville")]
            ]
        ]
  where
    candidates = tennesseeCapitalCandidates
    voters = tennesseeCapital

-- Helpers

namer candidates = show . (!!) candidates
namer1 candidates = map (namer candidates)
namer2 candidates = map (namer1 candidates)
namer1t candidates = map (Data.Bifunctor.bimap (namer candidates) (namer candidates))

systems :: (Voter a) => [(String, ElectoralSystem a)]
systems =
    [ ("FPTP", firstPastThePost)
    , ("Anti-plurality", antiPlurality)
    , ("TRS", twoRound)
    , ("IRV", instantRunoffVoting)
    , ("Borda", bordaCount)
    , ("Dowdall", dowdallSystem)
    ]

-- Compares an outcome of function f(c,v) given electoral system es.
compareOutcome :: (Voter a, Show b, Eq b) => (ElectoralSystem a -> b) -> ((TestName, ElectoralSystem a), b) -> TestTree
compareOutcome f_CV ((name, es), correctAnswer) =
    testCase name $ f_CV es @?= correctAnswer

testBySystem :: (Voter a, Show b, Eq b) => (ElectoralSystem a -> b) -> [b] -> [TestTree]
testBySystem f_CV = zipWith (curry (compareOutcome f_CV)) systems

testWinnerBySystem :: (Voter a) => [Candidate] -> [a] -> [String] -> TestTree
testWinnerBySystem candidates voters results =
    testGroup "(winner by system)" $
        testBySystem (\es -> namer candidates $ es candidates voters) results

testCondorcetWinner :: (Voter a) => [Candidate] -> [a] -> String -> TestTree
testCondorcetWinner candidates voters correctAnswer =
    testCase "(Condorcet winner)" $
        namer candidates (fromJust (condorcetWinner candidates voters)) @?= correctAnswer

testClones :: (Voter a) => [Candidate] -> [a] -> [[String]] -> TestTree
testClones candidates voters correctAnswer = testCase "(clones)" $ byNames @?= correctAnswer
  where
    byNumbers = clones candidates voters
    byNames = namer2 candidates byNumbers

testSpoilers :: (Voter a) => [Candidate] -> [a] -> [[String]] -> TestTree
testSpoilers candidates voters correctAnswers =
    testGroup "(spoilers)" $
        testBySystem (namer1 candidates . spoilers candidates voters) correctAnswers

testProxies :: (Voter a) => [Candidate] -> [a] -> [[(String, String)]] -> TestTree
testProxies candidates voters correctAnswers =
    testGroup "(spoilers)" $
        testBySystem (namer1t candidates . proxies candidates voters) correctAnswers