{-# LANGUAGE ImportQualifiedPost #-}

-- | Single peaked preferences-- See [here](https://en.wikipedia.org/wiki/Single_peaked_preferences) for a description of single peaked preferences.
module Psephology.SinglePeakedPreferences (singlePeakedVotersLim, singlePeakedVoters, formalize, singlePeakedVotersNormalLim, singlePeakedVotersNormal, singlePeakedVotersNormalCentered) where

import Control.Monad qualified
import Data.List
import Data.Random
import System.Random

import Psephology.Candidate
import Psephology.Voter

-- | @'singlePeakedVotersLim' limit n dims@ returns a list of @n@ voters, each represented using a spacial model with @dims@ dimensions.
-- The value on each dimension is a continuous value between 0 and @limit@. Uses a uniform random distribution.
singlePeakedVotersLim :: Double -> Int -> Int -> IO [[Double]]
singlePeakedVotersLim limit n dims =
    Control.Monad.replicateM n (Control.Monad.replicateM dims (randomRIO (0, limit)))

-- | Shortcut for 'singlePeakedVotersLim' that sets the limit to 100.
singlePeakedVoters :: Int -> Int -> IO [[Double]]
singlePeakedVoters = singlePeakedVotersLim 100

-- | @'singlePeakedVotersNormalLim' limit means n dims@ is similar to 'singlePeakedVotersLim', but a normal distribution is used instead.
-- @means@ indicates the center of gravity on the space. The mean of the normal distribution on each dimension is the respective member of
-- the list.
singlePeakedVotersNormalLim :: Double -> [Double] -> Int -> Int -> IO [[Double]]
singlePeakedVotersNormalLim limit means n dims =
    Control.Monad.replicateM n $ generateVoter limit means dims

generateVoter :: Double -> [Double] -> Int -> IO [Double]
generateVoter limit means dims =
    mapM (normDistValue limit) (take dims means)

normDistValue :: Double -> Double -> IO Double
normDistValue limit mean = do
    g <- newStdGen
    let m' = abs ((mean / limit) - 0.5) + 0.5
    let stdDev = limit / (m' * 12)
    let (v, _) = samplePure (Normal mean stdDev) g
    return $ max 0 $ min limit v

-- | Shortcut for 'singlePeakedVotersNormalLim' that sets the limit to 100.
singlePeakedVotersNormal :: [Double] -> Int -> Int -> IO [[Double]]
singlePeakedVotersNormal = singlePeakedVotersNormalLim 100

-- | Shortcut for 'singlePeakedVotersNormal' that places the center of gravity in the middle of the given range.
singlePeakedVotersNormalCentered :: Int -> Int -> IO [[Double]]
singlePeakedVotersNormalCentered n dims = singlePeakedVotersNormalLim 100 (replicate dims (100 / 2)) n dims

-- | Converts spacial voters (imaginary voters) into actual lists of preferences.
formalize :: [Candidate] -> [[Double]] -> [[Candidate]]
formalize _ [] = []
formalize candidates voters =
    map (\v -> sortOn (negate . utilityM v) candidates) voters