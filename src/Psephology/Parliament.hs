{- | This module allows the user to perform statistical analysis on a set of elections, called a parliament, across different voting systems. All
candidates in voters exist on the same space (same dimensions and bounds).
-}
module Psephology.Parliament where

import qualified Control.Monad
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

pathologies :: (Voter a) => Parliament a -> [[String]]
pathologies parliament =
    [ ""
    , "# paradoxs"
    , "# spoiled "
    , "# proxies*"
    , "Cond. fail"
    , "Maj. failu"
    , "MM failure"
    ]
        : [ [ systemName
            , show no_paradoxes
            , show $ length $ filter (\(Election candidates voters) -> not (null (spoilers candidates voters es))) parliament
            , show $ length $ filter (\(Election candidates voters) -> length (proxies candidates voters es) > 1) parliament
            , show $ length $ filter (\(Election candidates voters) -> condorcetFailure candidates voters es) parliament
            , show $ length $ filter (\(Election candidates voters) -> majorityFailure candidates voters es) parliament
            , show $ length $ filter (\(Election candidates voters) -> mutualMajorityFailure candidates voters es) parliament
            ]
          | (systemName, es) <- systems :: (Voter a) => [(String, ElectoralSystem a)]
          ]
  where
    no_paradoxes = length $ filter (\(Election candidates voters) -> isNothing $ condorcetWinner candidates voters) parliament