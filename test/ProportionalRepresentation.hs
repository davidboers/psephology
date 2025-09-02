module ProportionalRepresentation (testProportionalRepresentation) where

import Test.Tasty
import Test.Tasty.HUnit

import Psephology.ProportionalRepresentation.HighestAverages
import Psephology.ProportionalRepresentation.LargestRemainder
import Psephology.Quotas
import Psephology.SampleElections (proportionalRepresentation1)

testProportionalRepresentation :: TestTree
testProportionalRepresentation =
    testGroup
        "Proportional Representation"
        [ testQuotas
        , testResults
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