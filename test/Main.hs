{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Redistricting

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Runners.TAP

import Data.Bifunctor qualified
import Data.List (intercalate)
import Data.Maybe

import Psephology (Strategy (Strategy), bordaCount, bordaTally, dowdallWeight, firstPastThePost, icelandicWeight, traditionalBordaWeight)
import Psephology.BLT
import Psephology.Candidate
import Psephology.Condorcet
import Psephology.ElectoralSystem
import Psephology.ElectoralSystems.Condorcet
    ( dodgsonScore
    , kemenyOverallRanking
    )
import Psephology.ElectoralSystems.Runoff
import Psephology.McKelveySchofield
import Psephology.Parliament
import Psephology.Pathologies
import Psephology.SampleElections
import Psephology.Spoilers
import Psephology.Strategy.Abstention (abstain)
import Psephology.Strategy.Burying (bury)
import Psephology.Voter

useTAP :: Bool
useTAP = False

main :: IO ()
main = do
    _ <- utilitarianStatewide
    -- 10 minutes
    parliament <- generate 1000 2 100 10 100
    if useTAP
        then defaultMainWithIngredients [tapRunner] (tests parliament)
        else defaultMain $ tests parliament

tests :: Parliament [Double] -> TestTree
tests _ =
    testGroup
        "Tests"
        [ testTennesseeCapitalElection
        , testMcKelveySchofield
        , testRedistricting
        , testDodgsonScores
        , testStrategy
        -- Enable as you wish
        -- , testGeneratedParliament parliament
        ]

testTennesseeCapitalElection :: TestTree
testTennesseeCapitalElection =
    testGroup
        "Tennessee Capital election"
        [ testWinnerBySystem
            candidates
            voters
            [ "Memphis" -- FPTP
            , "Nashville" -- (Or Chattanooga) Anti-plurality
            , "Nashville" -- TRS
            , "Knoxville" -- IRV
            , "Nashville" -- Coombs
            , "Nashville" -- Borda
            , "Nashville" -- Dowdall
            , "Nashville" -- Icelandic Borda
            , "Nashville" -- Nanson
            , "Nashville" -- Baldwin
            , "Nashville" -- Tideman alternative
            , "Nashville" -- Minimax
            , "Nashville" -- Copeland-Llull
            , "Nashville" -- Black
            , "Nashville" -- Kemeny
            -- , "Nashville" -- Dodgson
            , "Nashville" -- Ranked pairs
            , "Nashville" -- Schulze
            ]
        , testCondorcetWinner candidates voters "Nashville"
        , testSmithSet candidates voters ["Nashville"]
        , testCase "(Kemeny: overall ranking)" $
            namer1 candidates (kemenyOverallRanking candidates voters)
                @?= ["Nashville", "Chattanooga", "Knoxville", "Memphis"]
        , testCase "(pairwise scores)" $
            condorcetMatrix numPreferOver candidates voters
                @?= [[100, 42, 42, 42], [58, 100, 68, 68], [58, 32, 100, 83], [58, 32, 17, 100]]
        , testCase "(Borda tally)" $
            bordaTally traditionalBordaWeight candidates voters @?= [126, 194, 173, 107]
        , testCase "(Dowdall tally)" $
            bordaTally dowdallWeight candidates voters @?= [56.5, 57.66666666666674, 50.50000000000001, 43.666666666666664]
        , testCase "(Icelandic Borda tally)" $
            bordaTally icelandicWeight candidates voters @?= [56.5, 73.5, 68.25, 51.75]
        , testCase "(.blt export)" $
            export candidates voters "Tennessee capital election"
                @?= "4 1\n42 1 2 3 4 0\n26 2 3 4 1 0\n15 3 4 2 1 0\n17 4 3 2 1 0\n0\n\"Memphis\"\n\"Nashville\"\n\"Chattanooga\"\n\"Knoxville\"\n\"Tennessee capital election\"\n"
        , testClones
            candidates
            voters
            [ ["Chattanooga", "Knoxville"]
            , ["Nashville", "Chattanooga", "Knoxville"]
            ]
        , testCase "(majority coalitions)" $
            majorityCoalitions candidates voters @?= [[1, 2, 3]]
        , testSpoilers
            candidates
            voters
            [ [] -- FPTP
            , ["Memphis"] -- Anti-plurality
            , ["Chattanooga", "Knoxville"] -- TRS
            , ["Memphis", "Nashville"] -- IRV
            , [] -- Coombs
            , [] -- Borda
            , [] -- Dowdall
            , [] -- Icelandic Borda
            , [] -- Nanson
            , [] -- Baldwin
            , [] -- Tideman alternative
            , [] -- Minimax
            , [] -- Copeland-Llull
            , [] -- Black
            , [] -- Kemeny
            -- , [] -- Dodgson
            , [] -- Ranked pairs
            , [] -- Schulze
            ]
        , testProxies
            candidates
            voters
            [ [("Memphis", "Nashville")] -- FPTP
            , [("Nashville", "Chattanooga")] -- Anti-plurality
            , [("Chattanooga", "Knoxville")] -- TRS
            , [("Chattanooga", "Knoxville")] -- IRV
            , [("Nashville", "Chattanooga")] -- Coombs
            , [("Nashville", "Chattanooga")] -- Borda
            , [("Memphis", "Nashville")] -- Dowdall
            , [("Nashville", "Chattanooga")] -- Icelandic Borda
            , [("Nashville", "Chattanooga")] -- Nanson
            , [("Nashville", "Chattanooga")] -- Baldwin
            , [("Nashville", "Chattanooga")] -- Tideman alternative
            , [("Nashville", "Chattanooga")] -- Minimax
            , [("Nashville", "Chattanooga")] -- Copeland-Llull
            , [("Nashville", "Chattanooga")] -- Black
            , [("Nashville", "Chattanooga")] -- Kemeny
            -- , [("Nashville", "Chattanooga")] -- Dodgson
            , [("Nashville", "Chattanooga")] -- Ranked pairs
            , [("Nashville", "Chattanooga")] -- Schulze
            ]
        , testForPathologies
            candidates
            voters
            [ [True, False, True] -- FPTP
            , [False, False, False] -- Anti-plurality
            , [False, False, False] -- TRS
            , [True, False, False] -- IRV
            , [] -- Coombs
            , [False, False, False] -- Borda
            , [False, False, False] -- Dowdall
            , [False, False, False] -- Icelandic Borda
            , [False, False, False] -- Nanson
            , [False, False, False] -- Baldwin
            , [False, False, False] -- Tideman alternative
            , [False, False, False] -- Minimax
            , [False, False, False] -- Copeland-Llull
            , [False, False, False] -- Black
            , [False, False, False] -- Kemeny
            -- , [False, False, False] -- Dodgson
            , [False, False, False] -- Ranked pairs
            , [False, False, False] -- Schulze
            ]
        ]
    where
        candidates = tennesseeCapitalCandidates
        voters = tennesseeCapital

testMcKelveySchofield :: TestTree
testMcKelveySchofield =
    testGroup
        "McKelvey-Schofield chaos theorem"
        [ testCase "(simple spoiler)" $
            findASpoiler p1 voters @?= basicSpoiler
        , testCase "(new majority)" $
            newMajority p1 voters basicSpoiler @?= [0, 2]
        , testCase "(spoiler potential)" $
            spoilerPotential p1 voters @?= 0.313033999999999
        , testCase "(spoiler path, p2>p1)" $
            thetaPath p1 voters [2, 8] @?= [[2, 8]]
        , testCase "(spoiler path 1)" $
            thetaPath p1 voters [0, 0] @?= [[1, 9], [0, 0]]
        , testCase "(spoiler path 2)" $
            thetaPath p1 voters [-3, 0] @?= [[1, 9], [-2.5, -2], [-3, 0]]
        ]
    where
        p1 = [9, 1]
        voters = [[0, 0], [10, 0], [10, 10]]
        basicSpoiler = [1, 9]

testGeneratedParliament :: Parliament [Double] -> TestTree
testGeneratedParliament parliament =
    testGroup
        "Generated parliament"
        [ testCase "Export generated voters to CSV" $
            do
                let csv =
                        unlines $
                            map (concatMap (\x -> show x ++ ",")) $
                                concatMap (\(Election _ voters) -> voters) parliament
                writeFile "test/heatmaps/voters.csv" csv
                True @?= True
        , testCase "Export generated electeds to CSV" $
            do
                let csv =
                        unlines $
                            zipWith
                                (\winner (Election candidates _) -> show $ candidates !! winner)
                                (winners instantRunoffVoting parliament)
                                parliament
                writeFile "test/heatmaps/elected.csv" csv
                True @?= True
        , testCase "Mass analysis" $
            do
                let csv = unlines $ map (intercalate ",") $ pathologies parliament
                writeFile "test/pathologies.csv" csv
                True @?= True
        ]

testDodgsonScores :: TestTree
testDodgsonScores =
    testGroup "Dodgson scores" $
        zipWith
            ( \c correctAnswer ->
                testCase (show c) $
                    dodgsonScore candidates voters c @?= correctAnswer
            )
            candidates
            [ 6 -- A
            , 3 -- B
            , 4 -- C
            ]
    where
        candidates = condorcetCycleCandidates
        voters = condorcetCycle

testStrategy :: TestTree
testStrategy =
    testGroup
        "Strategy"
        [ testCase "Borda, burying" $
            bury condorcetCycleCandidates condorcetCycle bordaCount 2 @?= Just bordaStrategy
        , testCase "Small referendum, abstaining" $
            abstain yesOrNo smallReferendum referendumFunc 1 @?= Just referendumStrategy
        ]
    where
        referendumFunc candidates voters = fromMaybe 1 $ turnoutRequirement 0.3 27 firstPastThePost candidates voters
        bordaStrategy = Strategy [[Categorical "B", Categorical "C", Categorical "A"]] 1
        referendumStrategy = Strategy (replicate 2 [Categorical "No", Categorical "Yes"]) 1

-- Helpers

namer :: Show b => [b] -> Int -> String
namer candidates = show . (!!) candidates
namer1 :: Show b => [b] -> [Int] -> [String]
namer1 candidates = map (namer candidates)
namer2 :: Show b => [b] -> [[Int]] -> [[String]]
namer2 candidates = map (namer1 candidates)
namer1t
    :: (Data.Bifunctor.Bifunctor p, Show b) => [b] -> [p Int Int] -> [p String String]
namer1t candidates = map (Data.Bifunctor.bimap (namer candidates) (namer candidates))

-- Compares an outcome of function f(c,v) given electoral system es.
compareOutcome
    :: (Voter a, Show b, Eq b)
    => (ElectoralSystem a -> b)
    -> ((TestName, ElectoralSystem a), b)
    -> TestTree
compareOutcome f_CV ((name, es), correctAnswer) =
    testCase name $ f_CV es @?= correctAnswer

testBySystem
    :: (Voter a, Show b, Eq b) => (ElectoralSystem a -> b) -> [b] -> [TestTree]
testBySystem f_CV = zipWith (curry (compareOutcome f_CV)) systems

testWinnerBySystem :: Voter a => [Candidate] -> [a] -> [String] -> TestTree
testWinnerBySystem candidates voters results =
    testGroup "(winner by system)" $
        testBySystem (\es -> namer candidates $ es candidates voters) results

testCondorcetWinner :: Voter a => [Candidate] -> [a] -> String -> TestTree
testCondorcetWinner candidates voters correctAnswer =
    testCase "(Condorcet winner)" $
        namer candidates (fromJust (condorcetWinner candidates voters))
            @?= correctAnswer

testSmithSet :: Voter a => [Candidate] -> [a] -> [String] -> TestTree
testSmithSet candidates voters correctAnswer =
    testCase "(Smith set)" $
        namer1 candidates (smithSet candidates voters)
            @?= correctAnswer

testClones :: Voter a => [Candidate] -> [a] -> [[String]] -> TestTree
testClones candidates voters correctAnswer = testCase "(clones)" $ byNames @?= correctAnswer
    where
        byNumbers = clones candidates voters
        byNames = namer2 candidates byNumbers

testSpoilers :: Voter a => [Candidate] -> [a] -> [[String]] -> TestTree
testSpoilers candidates voters correctAnswers =
    testGroup "(spoilers)" $
        testBySystem (namer1 candidates . spoilers candidates voters) correctAnswers

testProxies
    :: Voter a => [Candidate] -> [a] -> [[(String, String)]] -> TestTree
testProxies candidates voters correctAnswers =
    testGroup "(proxies)" $
        testBySystem (namer1t candidates . proxies candidates voters) correctAnswers

testForPathologiesBySystem
    :: Voter a
    => [Candidate]
    -> [a]
    -> (String, ElectoralSystem a)
    -> [Bool]
    -> TestTree
testForPathologiesBySystem candidates voters (systemName, es) correctAnswers =
    let actualWinner = es candidates voters
     in testGroup systemName $
            zipWith
                ( \(pathologyName, f) correctAnswer -> testCase pathologyName $ f actualWinner candidates voters @?= correctAnswer
                )
                [ ("Condorcet failure", condorcetFailure)
                , ("Majority failure", majorityFailure)
                , ("Mutual majority failure", mutualMajorityFailure)
                ]
                correctAnswers

testForPathologies :: Voter a => [Candidate] -> [a] -> [[Bool]] -> TestTree
testForPathologies candidates voters correctAnswers =
    testGroup "(pathologies)" $
        zipWith (testForPathologiesBySystem candidates voters) systems correctAnswers