{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Runners.TAP

import Data.Bifunctor qualified
import Data.List (find, intercalate, sort)
import Data.Maybe

import Psephology (Strategy (Strategy), bordaCount, firstPastThePost)
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
import Psephology.Redistricting.Utilitarian
import Psephology.SampleElections
import Psephology.Spoilers
import Psephology.Strategy.Abstention (abstain)
import Psephology.Strategy.Burying (bury)
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

testRedistricting :: TestTree
testRedistricting =
    testGroup
        "Redistricting"
        [ testCase "(utilitarian)" $
            noNonDissolved districts1 @?= noDistricts
        , testCase "(export journal1)" $
            do
                writeFile "test/redistricting/journal1.csv" $
                    unlines $
                        map (concatMap (++ ",")) csv1
                True @?= True
        , testCase "(export journal2)" $
            do
                writeFile "test/redistricting/journal2.csv" $
                    unlines $
                        map (concatMap (++ ",")) csv2
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
                                        : map (show . show . point) (sort precincts)
                            )
                            districts1
                True @?= True
        , testCase "(Gwinnett css)" $
            do
                writeFile "test/redistricting/gwinnett.css" $
                    unlines $
                        map
                            ( \precinct ->
                                "."
                                    ++ nameP precinct
                                    ++ "{ fill: "
                                    ++ ( case find
                                            (\(District _ precincts' _) -> nameP precinct `elem` map nameP precincts')
                                            districts2 of
                                            Just (District 2 _ _) -> "#3333ff"
                                            Just (District 51 _ _) -> "#f0ed4f"
                                            Just (District 74 _ _) -> "#0fb454"
                                            Just (District 95 _ _) -> "#33ff33"
                                            Just (District 124 _ _) -> "#be269e"
                                            _ -> "#dddddd"
                                       )
                                    ++ "; }"
                            )
                            gwinnettPrecincts
        ]
    where
        testPrecincts :: [Precinct]
        testPrecincts =
            [ Precinct "Unnamed" 1 [x, y]
            | x <- [0.5, 1.5 .. 9.5]
            , y <- [0.5, 1.5 .. 9.5]
            ]

        gwinnettPrecincts :: [Precinct]
        gwinnettPrecincts =
            [ Precinct "BAYCREEK-A" 3585 [33.889818, -83.9878974]
            , Precinct "BAYCREEK-B" 8447 [33.8160196, -83.958858]
            , Precinct "BAYCREEK-C" 9302 [33.9050831, -83.8995765]
            , Precinct "BAYCREEK-D" 6326 [33.9103313, -83.9496452]
            , Precinct "BAYCREEK-E" 5183 [33.8451058, -83.9412628]
            , Precinct "BAYCREEK-F" 6933 [33.8760407, -83.9616113]
            , Precinct "BAYCREEK-G" 9465 [33.8628516, -83.931562]
            , Precinct "BAYCREEK-H" 9916 [33.8765328, -83.9050861]
            , Precinct "BAYCREEK-I" 8833 [33.9260211, -83.9444786]
            , Precinct "BAYCREEK-J" 6205 [33.8435416, -83.96937]
            , Precinct "BAYCREEK-K" 5457 [33.9011826, -83.9718508]
            , Precinct "BERKSHIRE-A" 5703 [33.8932271, -84.1496682]
            , Precinct "BERKSHIRE-B" 11845 [33.8776441, -84.1775168]
            , Precinct "BERKSHIRE-D" 4101 [33.868216, -84.1028126]
            , Precinct "BERKSHIRE-E" 2722 [33.877295, -84.1416573]
            , Precinct "BERKSHIRE-F" 4403 [33.9039945, -84.108399]
            , Precinct "BERKSHIRE-G" 3325 [33.8466825, -84.1321556]
            , Precinct "BERKSHIRE-H" 4466 [33.8941438, -84.1716221]
            , Precinct "BERKSHIRE-J" 3743 [33.8678246, -84.1169868]
            , Precinct "BERKSHIRE-L" 6986 [33.9142183, -84.120079]
            , Precinct "BERKSHIRE-M" 2760 [33.8892216, -84.117783]
            , Precinct "BERKSHIRE-N" 4138 [33.857221, -84.1465624]
            , Precinct "BERKSHIRE-O" 3698 [33.885763, -84.0920034]
            , Precinct "BERKSHIRE-P" 3621 [33.9004652, -84.134341]
            , Precinct "BERKSHIRE-Q" 2741 [33.871126, -84.1537931]
            , Precinct "CATES-A" 3430 [33.8647111, -84.0053461]
            , Precinct "CATES-B" 2122 [33.8914019, -84.0684115]
            , Precinct "CATES-C" 4292 [33.8368834, -84.0009083]
            , Precinct "CATES-D" 5770 [33.8618825, -84.0511836]
            , Precinct "CATES-E" 4232 [33.8616479, -84.0634701]
            , Precinct "CATES-F" 2505 [33.8628461, -84.0279102]
            , Precinct "CATES-G" 4012 [33.8808455, -84.0224375]
            , Precinct "CATES-H" 6264 [33.8432275, -84.024743]
            , Precinct "CATES-I" 5403 [33.8837881, -84.0451492]
            , Precinct "CATES-J" 5210 [33.852748, -84.0804605]
            , Precinct "CATES-K" 3882 [33.9043004, -84.0529517]
            , Precinct "CATES-L" 5708 [33.8563568, -83.9908172]
            , Precinct "CATES-M" 4882 [33.9006071, -84.0381449]
            , Precinct "CATES-N" 3368 [33.8939635, -84.0047983]
            , Precinct "CATES-O" 2140 [33.8841421, -84.0683564]
            , Precinct "DACULA" 7312 [33.9876439, -83.9105131]
            , Precinct "DULUTH-A" 7637 [34.0213628, -84.126245]
            , Precinct "DULUTH-B" 5515 [33.9769392, -84.1271353]
            , Precinct "DULUTH-C" 3671 [34.0064702, -84.162836]
            , Precinct "DULUTH-D" 10275 [33.9783085, -84.1499937]
            , Precinct "DULUTH-E" 8147 [33.9933807, -84.1595608]
            , Precinct "DULUTH-F" 12394 [33.9738655, -84.0966092]
            , Precinct "DULUTH-G" 4815 [34.010063, -84.1221283]
            , Precinct "DULUTH-H" 4473 [34.020171, -84.162296]
            , Precinct "DULUTH-I" 7004 [34.0021646, -84.1059048]
            , Precinct "DULUTH-K" 6783 [33.9546932, -84.1457623]
            , Precinct "DUNCANS-A" 6540 [34.0650294, -83.911913]
            , Precinct "DUNCANS-B" 7903 [34.0694427, -83.8660054]
            , Precinct "DUNCANS-C" 4075 [34.0566395, -83.9005596]
            , Precinct "DUNCANS-D" 10195 [34.0971911, -83.8625174]
            , Precinct "GARNERS-A" 4673 [33.8451512, -84.0995503]
            , Precinct "GARNERS-B" 4391 [33.8181787, -84.0877152]
            , Precinct "GARNERS-C" 3095 [33.8288338, -84.1115658]
            , Precinct "GARNERS-D" 5081 [33.8056783, -84.0938775]
            , Precinct "GARNERS-F" 2640 [33.8358385, -84.1212987]
            , Precinct "GOODWINS-A" 5788 [33.9879019, -84.0352897]
            , Precinct "GOODWINS-B" 5375 [34.0230891, -84.0060567]
            , Precinct "GOODWINS-C" 9401 [33.9812482, -84.0593576]
            , Precinct "GOODWINS-D" 6328 [34.0021421, -84.0196182]
            , Precinct "GOODWINS-E" 7674 [34.0030653, -84.0574848]
            , Precinct "GOODWINS-F" 4618 [34.0287169, -84.0302508]
            , Precinct "GOODWINS-G" 4217 [33.9740572, -84.0280096]
            , Precinct "GOODWINS-H" 4230 [34.0230791, -84.0470859]
            , Precinct "GOODWINS-I" 3169 [33.9816617, -84.0502237]
            , Precinct "HARBINS-A" 11112 [33.938403, -83.8714539]
            , Precinct "HARBINS-B" 7369 [33.9794766, -83.8644208]
            , Precinct "HARBINS-C" 4204 [33.9186083, -83.835954]
            , Precinct "HOG-MOUNTAIN-A" 8042 [34.045449, -84.0163276]
            , Precinct "HOG-MOUNTAIN-B" 6314 [34.0095734, -83.945463]
            , Precinct "HOG-MOUNTAIN-C" 6691 [34.0501127, -83.962683]
            , Precinct "HOG-MOUNTAIN-D" 4737 [34.0265973, -83.9742231]
            , Precinct "LAWRENCEVILLE-A" 6050 [33.9448262, -83.9815648]
            , Precinct "LAWRENCEVILLE-B" 7989 [33.9140864, -83.9929366]
            , Precinct "LAWRENCEVILLE-C" 6172 [33.9933122, -83.9696585]
            , Precinct "LAWRENCEVILLE-D" 10811 [33.9649096, -83.982929]
            , Precinct "LAWRENCEVILLE-E" 3461 [33.9462713, -84.0112999]
            , Precinct "LAWRENCEVILLE-F" 5997 [33.9456761, -83.9409068]
            , Precinct "LAWRENCEVILLE-G" 4973 [33.928945, -84.0326378]
            , Precinct "LAWRENCEVILLE-H" 5875 [34.0021994, -83.9979153]
            , Precinct "LAWRENCEVILLE-I" 5968 [33.9831453, -84.0063628]
            , Precinct "LAWRENCEVILLE-J" 3562 [33.9186794, -84.0397274]
            , Precinct "LAWRENCEVILLE-K" 4331 [34.0147717, -83.9941712]
            , Precinct "LAWRENCEVILLE-L" 7250 [33.9146732, -84.0117177]
            , Precinct "LAWRENCEVILLE-M" 12001 [33.9694818, -83.9496471]
            , Precinct "LAWRENCEVILLE-N" 4532 [33.9340165, -84.0071546]
            , Precinct "MARTINS-A" 6977 [33.9567353, -84.066121]
            , Precinct "MARTINS-B" 5388 [33.9456094, -84.0483568]
            , Precinct "MARTINS-C" 4451 [33.9072875, -84.0852018]
            , Precinct "MARTINS-D" 5114 [33.9314484, -84.1114082]
            , Precinct "MARTINS-E" 13273 [33.9536353, -84.1070469]
            , Precinct "MARTINS-F" 5651 [33.9396051, -84.1293265]
            , Precinct "MARTINS-G" 4597 [33.9552398, -84.0245887]
            , Precinct "MARTINS-H" 9056 [33.9345146, -84.0648562]
            , Precinct "MARTINS-I" 8379 [33.9291163, -84.0972525]
            , Precinct "MARTINS-J" 8322 [33.9475762, -84.0907588]
            , Precinct "MARTINS-K" 9889 [33.9152335, -84.0639228]
            , Precinct "PINCKNEYVILLE-A1" 3338 [33.9757104, -84.249111]
            , Precinct "PINCKNEYVILLE-B" 11176 [33.8982183, -84.2068288]
            , Precinct "PINCKNEYVILLE-C" 2710 [33.9642062, -84.2315195]
            , Precinct "PINCKNEYVILLE-D" 5988 [33.9165115, -84.1792009]
            , Precinct "PINCKNEYVILLE-E" 4844 [33.9403949, -84.2570715]
            , Precinct "PINCKNEYVILLE-F" 6986 [33.9180147, -84.2327984]
            , Precinct "PINCKNEYVILLE-H" 6034 [33.9708347, -84.1955684]
            , Precinct "PINCKNEYVILLE-I" 5301 [33.9433818, -84.1611191]
            , Precinct "PINCKNEYVILLE-J" 8393 [33.9062732, -84.2127315]
            , Precinct "PINCKNEYVILLE-K" 9111 [33.9194429, -84.1592992]
            , Precinct "PINCKNEYVILLE-L" 2897 [33.9852037, -84.2347366]
            , Precinct "PINCKNEYVILLE-M" 3341 [33.9941726, -84.2163596]
            , Precinct "PINCKNEYVILLE-N" 8300 [33.9362721, -84.2408306]
            , Precinct "PINCKNEYVILLE-O" 9726 [33.8917728, -84.2155905]
            , Precinct "PINCKNEYVILLE-P" 3138 [33.966026, -84.249411]
            , Precinct "PINCKNEYVILLE-Q" 4995 [33.9352423, -84.13663]
            , Precinct "PINCKNEYVILLE-S" 6658 [33.9443672, -84.1858263]
            , Precinct "PINCKNEYVILLE-T" 6644 [33.9599364, -84.1751762]
            , Precinct "PINCKNEYVILLE-U" 3217 [33.9536613, -84.2660633]
            , Precinct "PINCKNEYVILLE-V" 3589 [33.8883758, -84.1859837]
            , Precinct "PINCKNEYVILLE-W" 8306 [33.9370609, -84.2116622]
            , Precinct "PINCKNEYVILLE-X" 4840 [33.9774504, -84.2058584]
            , Precinct "PINCKNEYVILLE-Y" 9540 [33.9058104, -84.1706759]
            , Precinct "PINCKNEYVILLE-Z" 2313 [33.9940352, -84.2399788]
            , Precinct "PINKCNEYVILLE-A" 7212 [33.933602, -84.1928967]
            , Precinct "PUCKETTS-A" 8054 [34.1129566, -83.9434912]
            , Precinct "PUCKETTS-B" 4486 [34.0889442, -83.973268]
            , Precinct "PUCKETTS-C" 6388 [34.0998056, -83.9153111]
            , Precinct "PUCKETTS-D" 5310 [34.0757572, -83.9343002]
            , Precinct "PUCKETTS-E" 8927 [34.0716606, -83.961447]
            , Precinct "ROCKBRIDGE-A" 5732 [33.7927546, -84.0467625]
            , Precinct "ROCKBRIDGE-B" 6339 [33.8338007, -84.0443693]
            , Precinct "ROCKBRIDGE-C" 7179 [33.8151462, -84.0470011]
            , Precinct "ROCKBRIDGE-D" 4853 [33.8119302, -84.0050123]
            , Precinct "ROCKBRIDGE-E" 7169 [33.8267236, -84.0595636]
            , Precinct "ROCKBRIDGE-F" 7998 [33.7891683, -84.011047]
            , Precinct "ROCKBRIDGE-G" 8459 [33.7729458, -84.0388929]
            , Precinct "ROCKYCREEK-A" 8636 [34.0326726, -83.8785003]
            , Precinct "ROCKYCREEK-B" 7660 [34.0377554, -83.9313674]
            , Precinct "ROCKYCREEK-C" 6942 [34.0139168, -83.9220142]
            , Precinct "SUGAR-HILL-A" 9344 [34.0873182, -84.0020166]
            , Precinct "SUGAR-HILL-B" 9283 [34.1241782, -84.050104]
            , Precinct "SUGAR-HILL-C" 7481 [34.1501347, -84.0575652]
            , Precinct "SUGAR-HILL-D" 8579 [34.1263295, -83.9927198]
            , Precinct "SUGAR-HILL-E" 5627 [34.0875286, -84.0277031]
            , Precinct "SUGAR-HILL-F" 6984 [34.0970318, -84.0587765]
            , Precinct "SUGAR-HILL-G" 3971 [34.1086611, -84.0668378]
            , Precinct "SUWANEE-A" 2772 [34.0617125, -84.0670736]
            , Precinct "SUWANEE-B" 7364 [34.0636681, -84.0444275]
            , Precinct "SUWANEE-C" 9256 [34.0247114, -84.0731875]
            , Precinct "SUWANEE-D" 7036 [34.0994645, -84.0967614]
            , Precinct "SUWANEE-E" 6267 [34.0742392, -84.077551]
            , Precinct "SUWANEE-F" 6060 [34.0490726, -84.0392913]
            , Precinct "SUWANEE-G" 5953 [34.0400745, -84.1011224]
            , Precinct "SUWANEE-H" 5275 [34.0735075, -84.0959355]
            ]

        noDistricts = 5

        (csv1, districts1) =
            thenEqualizeVerbose 100 1 $
                reduceVerbose noDistricts (precinctsToDistricts testPrecincts)

        (csv2, districts2) =
            thenEqualizeVerbose 100 1.07 $
                reduceVerbose noDistricts (precinctsToDistricts gwinnettPrecincts)

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
            bury condorcetCycleCandidates condorcetCycle bordaCount 2 @?= Nothing
        , testCase "Small referendum, abstaining" $
            abstain yesOrNo smallReferendum referendumFunc 1 @?= Just referendumStrategy
        ]
    where
        referendumFunc candidates voters = fromMaybe 1 $ turnoutRequirement 0.3 27 firstPastThePost candidates voters
        referendumStrategy = Strategy [[Categorical "No", Categorical "Yes"], [Categorical "No", Categorical "Yes"]] 1

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