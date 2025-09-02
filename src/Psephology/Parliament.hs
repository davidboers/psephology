-- | This module allows the user to perform statistical analysis on a set of elections, called a parliament, across different voting systems. All
-- candidates in voters exist on the same space (same dimensions and bounds).
module Psephology.Parliament where

import qualified Control.Monad
import Data.List (foldl')
import Data.Maybe (isNothing)

import Psephology.Candidate
import Psephology.Condorcet (condorcetWinner, smithSet)
import Psephology.ElectoralSystem
import Psephology.Pathologies
import Psephology.SinglePeakedPreferences
import Psephology.Spoilers
import Psephology.Voter (Voter)

data Election a = Election [Candidate] [a]

type Parliament a = [Election a]

winners :: ElectoralSystem a -> Parliament a -> [Int]
winners es = map (\(Election candidates voters) -> es candidates voters)

generate :: Int -> Int -> Int -> Int -> Double -> IO (Parliament [Double])
generate n dims no_voters no_candidates limit =
    Control.Monad.replicateM n $ do
        let center = replicate dims (limit / 2)
        cs <- singlePeakedVotersNormalLim limit center no_candidates dims
        let candidates = map Spacial cs
        voters <- singlePeakedVotersNormalLim limit center no_voters dims
        return $ Election candidates voters

pathologies :: Voter a => Parliament a -> [[String]]
pathologies parliament = do
    let header =
            [ ""
            , "# paradoxes"
            , "# w/clones"
            , "# spoiled"
            , "# spoiled by non-Smith candidates"
            , "# w/non-winning proxy pairs"
            , "# Condorcet failures"
            , "# Majority failures"
            , "# Mutual majority failures"
            , "# Smith failures"
            ]
    let systems' = systems :: [(String, ElectoralSystem [Double])]
    let t0 = replicate (length systems') $ replicate 9 0
    let t = foldl' pathologiesElection t0 parliament
    let tstrings = map (map show) t
    let withRowLabels = zipWith (:) (map fst systems') tstrings
    header : withRowLabels

pathologiesElection :: Voter a => [[Int]] -> Election a -> [[Int]]
pathologiesElection t e@(Election candidates voters) =
    let isParadox = fromEnum $ isNothing $ condorcetWinner candidates voters
        hasClones = fromEnum $ length (clones candidates voters) > 1
     in zipWith (zipWith (+)) t $ map (\(_, es) -> isParadox : hasClones : pathologiesElectionES e es) systems

pathologiesElectionES :: Voter a => Election a -> ElectoralSystem a -> [Int]
pathologiesElectionES (Election candidates voters) es =
    let actualWinner = es candidates voters
        listSpoilers = spoilers candidates voters es
        smith = smithSet candidates voters
     in map
            fromEnum
            [ not $ null listSpoilers
            , length (proxies candidates voters es) > 1
            , not $ any (`elem` smith) listSpoilers
            , condorcetFailure actualWinner candidates voters
            , majorityFailure actualWinner candidates voters
            , mutualMajorityFailure actualWinner candidates voters
            , smithFailure actualWinner candidates voters
            ]
