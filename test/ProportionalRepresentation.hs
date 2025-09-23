module ProportionalRepresentation (testProportionalRepresentation) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Psephology.ProportionalRepresentation.HighestAverages
import Psephology.ProportionalRepresentation.LargestRemainder
import Psephology.Quotas
import Psephology.SampleElections (proportionalRepresentation1)
import Psephology.ProportionalRepresentation

testProportionalRepresentation :: TestTree
testProportionalRepresentation =
    testGroup
        "Proportional Representation"
        [ testQuotas
        , testResults
        , testCompensation
        , testIceland
        , testRule "Relative positions rule" testCheckRelative
        , testRule "Correct number of seats allocated" testCorrectSeats
        ]
        
-- Rules

methods :: [(String, [Int] -> Int -> [Int])]
methods =
    [ ("Adams"              , highestAverages adams)
    , ("Dean"               , highestAverages dean)
    , ("D'Hondt"            , highestAverages dhondt)
    , ("Huntington-Hill"    , highestAverages huntingtonHill)
    , ("Sainte-Laguë"       , highestAverages sainteLague)
    , ("Macanese"           , highestAverages macanese)
    , ("Hare"               , largestRemainder hare)
    , ("Droop"              , largestRemainder droop)
    , ("Hagenbach-Bischoff" , largestRemainder hagenbachBischoff)
    , ("Imperiali"          , largestRemainder imperiali)
    ]

testRule :: Testable a => String -> (([Int] -> Int -> [Int]) -> a) -> TestTree
testRule ruleName rule =
    let underMethod methodName m = testProperty methodName (rule m) in
    testGroup ruleName $ map (uncurry underMethod) methods

testCheckRelative :: ([Int] -> Int -> [Int]) -> [Int] -> Int -> Bool
testCheckRelative m votes numSeats = 
    let votes' = map abs votes in
    checkRelative votes' (m votes' numSeats)

testCorrectSeats :: ([Int] -> Int -> [Int]) -> [Int] -> Int -> Bool
testCorrectSeats _ []    _        = True -- Annoying
testCorrectSeats m votes numSeats = 
    let votes' = map abs votes in
    correctNumSeats numSeats (m votes' numSeats)

-- Cases

testQuotas :: TestTree
testQuotas =
    testGroup
        "(quotas; 1000 votes, 10 seats)"
        [ testCase "Hare"               $ hare              1000 10 @?= 100
        , testCase "Droop"              $ droop             1000 10 @?= 91
        , testCase "Hagenbach-Bischoff" $ hagenbachBischoff 1000 10 @?= 90
        , testCase "Imperiali"          $ imperiali         1000 10 @?= 83
        ]

testResults :: TestTree
testResults =
    testGroup
        "(results)"
        [ testCase "Adams" $
            highestAverages adams              proportionalRepresentation1 20 @?= [5, 5, 3, 2, 2, 2, 1]
        , testCase "Dean" $
            highestAverages dean               proportionalRepresentation1 20 @?= [6, 5, 3, 2, 2, 1, 1]
        , testCase "D'Hondt" $
            highestAverages dhondt             proportionalRepresentation1 20 @?= [6, 6, 3, 2, 1, 1, 1]
        , testCase "Huntington-Hill" $
            highestAverages huntingtonHill     proportionalRepresentation1 20 @?= [6, 5, 3, 2, 2, 1, 1]
        , testCase "Sainte-Laguë" $
            highestAverages sainteLague        proportionalRepresentation1 20 @?= [6, 5, 3, 2, 2, 1, 1]
        , testCase "Macanese" $
            highestAverages macanese           proportionalRepresentation1 20 @?= [4, 4, 3, 3, 2, 2, 2]
        , testCase "Hare quota" $
            largestRemainder hare              proportionalRepresentation1 20 @?= [6, 5, 3, 2, 2, 1, 1]
        , testCase "Droop quota" $
            largestRemainder droop             proportionalRepresentation1 20 @?= [6, 5, 3, 2, 2, 1, 1]
        , testCase "Hagenbach-Bischoff quota" $
            largestRemainder hagenbachBischoff proportionalRepresentation1 20 @?= [6, 5, 3, 2, 2, 1, 1]
        , testCase "Imperiali quota" $
            largestRemainder imperiali         proportionalRepresentation1 20 @?= [6, 6, 3, 2, 1, 1, 1]
        ]

testCompensation :: TestTree
testCompensation =
    testGroup "(compensation; New Zealand)"
        [ testCase "List seats"   $ listSeats           (highestAverages sainteLague) electorateSeats listVotes 49  @?= [19,   5, 14, 11, 1, 0, 0]
        , testCase "w/o overhang" $ listSeatsWOOverhang (highestAverages sainteLague) electorateSeats listVotes 49  @?= [18,   5, 14, 11, 1, 0, 0]
        , testCase "Leveling"     $ levelStartingWith   (highestAverages sainteLague) electorateSeats listVotes 120 @?= [107, 57, 24, 20, 3, 2, 1]
        ]
    where 
        electorateSeats = [41, 27, 0, 0, 1, 1, 1]
        listVotes = [1131501, 604535, 257359, 208300, 31849, 16689, 5286]

-- More things that should be in the standard library!
firsts :: [(a, b)] -> [a]
firsts = map fst

seconds :: [(a, b)] -> [b]
seconds = map snd

testIceland :: TestTree
testIceland =
    testGroup "(Iceland 2024)"
        [ testCase "Reykjavik North" $ highestAverages dhondt (firsts reykjavikNorth) 9  @?= seconds reykjavikNorth
        , testCase "Reykjavik South" $ highestAverages dhondt (firsts reykjavikSouth) 9  @?= seconds reykjavikSouth
        , testCase "Southwest"       $ highestAverages dhondt (firsts southwest     ) 12 @?= seconds southwest
        , testCase "Northwest"       $ highestAverages dhondt (firsts northwest     ) 6  @?= seconds northwest
        , testCase "Northeast"       $ highestAverages dhondt (firsts northeast     ) 9  @?= seconds northeast
        , testCase "South"           $ highestAverages dhondt (firsts south         ) 9  @?= seconds south
        ]
    where
        reykjavikNorth =
            [ (9653, 3) -- S
            , (6459, 2) -- D
            , (6043, 2) -- C
            , (4400, 1) -- F
            , (3284, 1) -- M
            , (2194, 0) -- J
            , (2006, 0) -- P
            , (1492, 0) -- B
            , (1080, 0) -- V
            , (367,  0) -- L
            , (42,   0) -- Y
            ]

        reykjavikSouth =
            [ (8541, 3) -- S
            , (6581, 2) -- C
            , (6553, 2) -- D
            , (5022, 1) -- F
            , (3917, 1) -- M
            , (2091, 0) -- J
            , (1638, 0) -- B
            , (1445, 0) -- P
            , (1080, 0) -- V
            , (388,  0) -- L
            ]

        southwest =
            [ (14997, 3) -- D
            , (12829, 3) -- C
            , (12324, 3) -- S
            , (7689,  2) -- M
            , (7014,  1) -- F
            , (3792,  0) -- B
            , (1820,  0) -- J
            , (1778,  0) -- P
            , (987,   0) -- V
            , (728,   0) -- L
            ]

        northwest = 
            [ (3249, 1) -- D
            , (3023, 1) -- F
            , (2871, 1) -- S
            , (2670, 1) -- M
            , (2405, 1) -- B
            , (2286, 1) -- C
            , (620,  0) -- J
            , (486,  0) -- V
            , (322,  0) -- P
            , (143,  0) -- L
            ]
        
        northeast =
            [ (5183, 2) -- S
            , (3818, 2) -- M
            , (3652, 2) -- D
            , (3475, 1) -- F
            , (3445, 1) -- B
            , (2296, 1) -- C
            , (924,  0) -- J
            , (920,  0) -- V
            , (438,  0) -- P
            , (183,  0) -- L
            ]

        south =
            [ (6354, 2) -- F
            , (6233, 2) -- D
            , (5519, 2) -- S
            , (4322, 1) -- M
            , (3806, 1) -- B
            , (3571, 1) -- C
            , (773,  0) -- J
            , (422,  0) -- P
            , (421,  0) -- V
            , (406,  0) -- L
            ]