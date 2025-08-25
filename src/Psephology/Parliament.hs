-- | This module allows the user to perform statistical analysis on a set of elections, called a parliament, across different voting systems. All
-- candidates in voters exist on the same space (same dimensions and bounds).
module Psephology.Parliament where

import qualified Control.Monad
import Data.List (foldl')
import Data.Maybe (isNothing)

import Psephology.Candidate
import Psephology.Condorcet (condorcetWinner)
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
            , "# paradoxs"
            , "# spoiled "
            , "# proxies*"
            , "Cond. fail"
            , "Maj. failu"
            , "MM failure"
            , "Smith fail"
            ]
    let systems' = systems :: [(String, ElectoralSystem [Double])]
    let t0 = replicate (length systems') $ replicate 7 0
    let t = foldl' pathologiesElection t0 parliament
    let tstrings = map (map show) t
    let withRowLabels = zipWith (:) (map fst systems') tstrings
    header : withRowLabels

pathologiesElection :: Voter a => [[Int]] -> Election a -> [[Int]]
pathologiesElection t e@(Election candidates voters) =
    let isParadox = fromEnum $ isNothing $ condorcetWinner candidates voters
     in zipWith (zipWith (+)) t $ map (\(_, es) -> isParadox : pathologiesElectionES e es) systems

pathologiesElectionES :: Voter a => Election a -> ElectoralSystem a -> [Int]
pathologiesElectionES (Election candidates voters) es =
    let actualWinner = es candidates voters
     in map
            fromEnum
            [ not $ null $ spoilers candidates voters es
            , length (proxies candidates voters es) > 1
            , condorcetFailure actualWinner candidates voters
            , majorityFailure actualWinner candidates voters
            , mutualMajorityFailure actualWinner candidates voters
            , smithFailure actualWinner candidates voters
            ]
