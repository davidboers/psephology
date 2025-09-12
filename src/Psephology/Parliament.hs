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

-- | @'generate' limit center no_candidates no_voters dims n@ generates @n@ elections from
-- 'singlePeakedVotersNormLim' with the given settings.
generate :: Double -> [Double] -> Int -> Int -> Int -> Int -> IO (Parliament [Double])
generate limit center no_candidates no_voters dims n =
    Control.Monad.replicateM n $ do
        cs <- singlePeakedVotersNormalLim limit center no_candidates dims
        let candidates = map Spacial cs
        voters <- singlePeakedVotersNormalLim limit center no_voters dims
        return $ Election candidates voters

-- | Combines two @'Parliament'@s by merging the candidate and voter lists of each
-- respective @'Election'@. Importantly, this does not simply concatenate the two lists of
-- @'Election'@s. This function is intended for where a parliament is to be generated with variable
-- settings.
merge :: Parliament a -> Parliament a -> Parliament a
merge (x : xs) (y : ys) =
    let (Election candidates1 voters1) = x; (Election candidates2 voters2) = y
     in Election (candidates1 ++ candidates2) (voters1 ++ voters2)
            : merge xs ys
merge [] rhs = rhs
merge lhs [] = lhs

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
    let t0 = replicate (length systems') $ replicate (length header + 1) 0
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
