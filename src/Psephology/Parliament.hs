{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE CPP #-}
-- | This module allows the user to perform statistical analysis on a set of elections, called a parliament, across different voting systems. All
-- candidates in voters exist on the same space (same dimensions and bounds).
module Psephology.Parliament
    ( Parliament
    , Election(..)
    , winners
    , generate
    , merge
    , pathologies
    ) where

import qualified Control.Monad
import Data.Maybe (isNothing)

#if __GLASGOW_HASKELL__ < 910
import Data.List (foldl')
#endif

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
        candidates <- map Spacial <$> singlePeakedVotersNormalLim limit center no_candidates dims
        voters <- singlePeakedVotersNormalLim limit center no_voters dims
        return $ Election candidates voters

-- | Combines two @'Parliament'@s by merging the candidate and voter lists of each
-- respective @'Election'@. Importantly, this does not simply concatenate the two lists of
-- @'Election'@s. This function is intended for where a parliament is to be generated with variable
-- settings.
merge :: Parliament a -> Parliament a -> Parliament a
merge xs ys = zipWith mergeElections xs ys 
    ++ drop (length ys) xs 
    ++ drop (length xs) ys

mergeElections :: Election a -> Election a -> Election a
mergeElections (Election candidates1 voters1) (Election candidates2 voters2) =
    Election (candidates1 ++ candidates2) (voters1 ++ voters2)

pathologies :: Voter a => Parliament a -> [[String]]
pathologies parliament =
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
        systemNames = map fst (systems :: [(String, ElectoralSystem [Double])])
        t0 = map (const $ replicate (length header + 1) 0) systemNames
        t = foldl' pathologiesElection t0 parliament
        withRowLabels = zipWith (:) systemNames $ map (map show) t
     in header : withRowLabels

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
     in map fromEnum
            [ not $ null listSpoilers
            , length (proxies candidates voters es) > 1
            , not $ any (`elem` smith) listSpoilers
            , condorcetFailure actualWinner candidates voters
            , majorityFailure actualWinner candidates voters
            , mutualMajorityFailure actualWinner candidates voters
            , smithFailure actualWinner candidates voters
            ]
