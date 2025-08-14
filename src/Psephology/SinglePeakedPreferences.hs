{-# LANGUAGE ImportQualifiedPost #-}

-- Single peaked preferences
-- See [here](https://en.wikipedia.org/wiki/Single_peaked_preferences) for a description of single peaked preferences.
module Psephology.SinglePeakedPreferences (singlePeakedVotersContLim, singlePeakedVotersCont, formalize) where

import Control.Monad qualified
import Data.List
import System.Random

import Psephology.Candidate
import Psephology.Voter

{- | @'singlePeakedVotersContLim' limit n dims@ returns a list of @n@ voters, each represented using a spacial model with @dims@ dimensions.
The value on each dimension is a continuous value between 0 and @limit@.
-}
singlePeakedVotersContLim :: Double -> Int -> Int -> IO [[Double]]
singlePeakedVotersContLim limit n dims =
    Control.Monad.replicateM n (Control.Monad.replicateM dims (randomRIO (1.0, limit)))

-- Shortcut for 'singlePeakedVotersContLim' that sets the limit to 100.
singlePeakedVotersCont :: Int -> Int -> IO [[Double]]
singlePeakedVotersCont = singlePeakedVotersContLim 100

-- Converts spacial voters (imaginary voters) into actual lists of preferences.
formalize :: [Candidate] -> [[Double]] -> [[Candidate]]
formalize _ [] = []
formalize candidates (v : voters) =
    sortOn (dist v) candidates : formalize candidates voters
