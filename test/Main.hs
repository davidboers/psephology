{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Runners.TAP

import Data.Bifunctor qualified
import Data.Maybe
import GHC.OldList (intercalate)

import Psephology.BLT
import Psephology.Candidate
import Psephology.Condorcet
import Psephology.ElectoralSystem
import Psephology.ElectoralSystems.Runoff
import Psephology.McKelveySchofield
import Psephology.Parliament
import Psephology.Pathologies
import Psephology.Redistricting.Utilitarian
import Psephology.SampleElections
import Psephology.Spoilers
import Psephology.Voter

useTAP :: Bool
useTAP = False

main :: IO ()
main = do
    parliament <- generate 1000 2 100 5 100
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
            , "Nashville" -- Borda
            , "Nashville" -- Dowdall
            , "Nashville" -- Icelandic Borda
            , "Nashville" -- Nanson
            , "Nashville" -- Baldwin
            , "Nashville" -- Copeland-Llull
            , "Nashville" -- Ranked pairs
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
            , [] -- Copeland-Llull
            , [] -- Ranked pairs
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
            , [("Nashville", "Chattanooga")] -- Copeland-Llull
            , [("Nashville", "Chattanooga")] -- Ranked pairs
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
            , [False, False, False] -- Copeland-Llull
            , [False, False, False] -- Ranked pairs
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

testRedistricting :: TestTree
testRedistricting =
    testGroup
        "Redistricting"
        [ testCase "(utilitarian)" $
            noNonDissolved districts @?= noDistricts
        , testCase "(export journal)" $
            do
                writeFile "test/redistricting/journal1.csv" $
                    unlines $
                        map (concatMap (++ ",")) csv
                True @?= True
        , testCase "(export districts)" $
            do
                writeFile "test/redistricting/test1.csv" $
                    unlines $
                        map
                            ( \d@(District idD precincts _) ->
                                concatMap (++ ",") $
                                    show idD
                                        : show (populationD d)
                                        : map (\(Precinct _ p) -> show $ show p) precincts
                            )
                            districts
                True @?= True
        ]
  where
    testPrecincts :: [Precinct]
    testPrecincts =
        [ Precinct 1 [x, y]
        | x <- [0.5, 1.5 .. 9.5]
        , y <- [0.5, 1.5 .. 9.5]
        ]

    noDistricts = 5

    (csv, districts) = reduceVerbose (Just 100) noDistricts (precinctsToDistricts testPrecincts)

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
    testGroup systemName $
        zipWith
            ( \(pathologyName, f) correctAnswer -> testCase pathologyName $ f candidates voters es @?= correctAnswer
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