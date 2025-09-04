module ProportionalRepresentation (testProportionalRepresentation) where

import Test.Tasty
import Test.Tasty.HUnit

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
        ]

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