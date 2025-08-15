{- | This module allows the user to perform statistical analysis on a set of elections, called a parliament, across different voting systems. All
candidates in voters exist on the same space (same dimensions and bounds).
-}
module Psephology.Parliament where

import qualified Control.Monad

import Psephology.Candidate
import Psephology.ElectoralSystem
import Psephology.SinglePeakedPreferences (singlePeakedVotersLim)

data Election = Election [Candidate] [[Double]]

type Parliament = [Election]

winners :: ElectoralSystem [Double] -> Parliament -> [Int]
winners es = map (\(Election candidates voters) -> es candidates voters)

generate :: Int -> Int -> Int -> Int -> Int -> IO Parliament
generate n dims no_voters no_candidates limit =
    Control.Monad.replicateM n $ do
        cs <- singlePeakedVotersLim (fromIntegral limit) no_candidates dims
        let candidates = map Spacial cs
        voters <- singlePeakedVotersLim (fromIntegral limit) no_voters dims
        return $ Election candidates voters