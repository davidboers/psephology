{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Psephology.Fraud (
    -- * Fraud detection scheme
      detectFraud
    , FraudMessage
    , report
    , reportE
    , over100Message
    , clusteringMessage
    , makeMessages

    -- * Returns
    , Return(..)
    , voteShareX
    , turnoutRate
    , xShareOfRegVoters
    , aggregateVoteShareX

    -- * Impossible returns
    , xOver100
    , tOver100
    , xOver100R

    -- * Turnout vs. vote share
    , turnoutAtOrBelow
    , cumulativeShareAt
    , roundVoteShares
    ) where

import Data.List (elemIndices, genericLength)
import Data.Either

-- Data type

-- | A single return, from a precinct or in aggregate, for a candidate, party, or ballot option x.
data Return = Return
    { votesX :: Int
    -- ^ Number of votes for x.
    , validVotes :: Int
    -- ^ Number of "valid" votes cast.
    , regVoters :: Int
    -- ^ Number of registered voters.
    }

-- | 'votesX' / 'validVotes'
voteShareX :: Return -> Double
voteShareX (Return {votesX, validVotes}) =
    fromIntegral votesX / fromIntegral validVotes

-- | 'validVotes' / 'regVoters'
turnoutRate :: Return -> Double
turnoutRate (Return {validVotes, regVoters}) =
    fromIntegral validVotes / fromIntegral regVoters

-- | 'votesX' / 'regVoters'
xShareOfRegVoters :: Return -> Double
xShareOfRegVoters (Return {votesX, regVoters}) =
    fromIntegral votesX / fromIntegral regVoters

-- | Vote share for x across the list of returns.
aggregateVoteShareX :: [Return] -> Double
aggregateVoteShareX returns =
    fromIntegral (sum (map votesX returns)) /
        fromIntegral (sum (map validVotes returns))

-- >100%

-- | Returns @Nothing@ if the list is empty.
maybeAList :: [a] -> Maybe [a]
maybeAList [] = Nothing
maybeAList l  = Just l

-- | Returns a list of returns where x received more than 100% of the vote, or @Nothing@ if there 
-- are no such returns.
xOver100 :: [Return] -> Maybe [Return]
xOver100 returns =
    maybeAList $ filter ((>1) . voteShareX) returns

-- | Returns a list of returns where more than 100% turnout was reported, or @Nothing@ if there are 
-- no such returns.
tOver100 :: [Return] -> Maybe [Return]
tOver100 returns =
    maybeAList $ filter ((>1) . turnoutRate) returns

-- | Returns a list of returns where x received more votes than there were registered voters, or 
-- @Nothing@ if there are no such returns.
xOver100R :: [Return] -> Maybe [Return]
xOver100R returns =
    maybeAList $ filter ((>1) . xShareOfRegVoters) returns

-- Turnout vs. vote share

turnoutAtOrBelow :: Double -> Return -> Bool
turnoutAtOrBelow threshold ret =
    turnoutRate ret <= threshold

cumulativeShareAt :: [Return] -> Double -> Double
cumulativeShareAt returns share =
    aggregateVoteShareX $ filter (turnoutAtOrBelow share) returns

-- Integer vote share

binFactor :: Double
binFactor = 1000

-- Unused for now
binSize :: Double
binSize = 1 / binFactor

sharesAt :: Double -> [Double]
sharesAt factor = let stepSize = 1 / factor in [0, stepSize..1]

bin :: Double -> Double
bin r = fromIntegral (round (r * binFactor)) / binFactor

-- | Returns a histogram of the number of returns in 0.1% bins, filtered by whether the bin is round.
roundVoteShares :: Double -> [Return] -> [Int]
roundVoteShares factor returns =
    let shares = map (bin . voteShareX) returns in
    [ length $ elemIndices i shares
    | i <- sharesAt factor
    ]

-- Analyze

-- | A fraud warning.
data FraudMessage
    = Over100 Int String String
    | Clustering String Double Double
    | Summary Int Int

showPct :: Double -> String
showPct p = show (round (p * 100)) <> "%"

report :: FraudMessage -> String
report (Over100 noReturns lhs rhs) = 
    show noReturns <> " returns reported more " <> lhs <> " than " <> rhs <> "."

report (Clustering clusterType lhs rhs) = 
    "There are a suspiciously high proportion of returns for x (" <> showPct lhs <> ") clustered around " <> 
        clusterType <> ". The predicted proportion (in a uniform distribution) would be " <> showPct rhs <> "."

report (Summary lhs rhs) = 
    show lhs <> " of " <> show rhs <> " tests detected fraud."

reportE :: Either String FraudMessage -> String
reportE (Left noFraud)  = "OK             " <> noFraud
reportE (Right warning) = "FRAUD DETECTED " <> report warning

over100Message :: Maybe [Return] -> String -> String -> Either String FraudMessage
over100Message Nothing  lhs rhs = Left  $ lhs <> "/" <> rhs <> ": No fraud."
over100Message (Just l) lhs rhs = Right $ Over100 (length l) lhs rhs

clusteringMessage :: [Return] -> Either String FraudMessage
clusteringMessage returns 
    | p < 0.05  = Left "Clustering: No fraud."
    | otherwise = Right $ Clustering "integer percentages" inIntBins expectedIntReturns
    where
        factor = 100
        inIntBins = fromIntegral $ sum $ roundVoteShares factor returns :: Double
        expectedIntReturns = genericLength (sharesAt factor) / genericLength (sharesAt binFactor) -- What about if a normal distribution?
        p = abs (1 - (inIntBins / (fromIntegral (length returns) - inIntBins)))

makeMessages :: [Return] -> [Either String FraudMessage]
makeMessages returns =
    [ over100Message (xOver100 returns) "votes for x" "ballots cast"
    , over100Message (tOver100 returns) "ballots cast" "registered voters"
    , over100Message (xOver100R returns) "votes for x" "registered voters"
    , clusteringMessage returns
    ]

detectFraud :: [Return] -> String
detectFraud returns =
    let messages = makeMessages returns 
        reports = map reportE messages
        pad = maximum (map length reports) - 4
        border = "||" <> replicate pad '=' <> "||"
        summary 
            | all isLeft messages = reportE $ Left    "None of the tests uncovered evidence of fraud."
            | otherwise           = reportE $ Right $ Summary (length $ rights messages) (length messages)
     in
    unlines [border, unlines reports, border, summary]