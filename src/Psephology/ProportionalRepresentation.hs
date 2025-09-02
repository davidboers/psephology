-- | Tools for analyzing and calculating proportional representation.
module Psephology.ProportionalRepresentation where

import Data.List (findIndices)

-- | @'gallagherIndex' votes seats@ returns the index of relative disproportionality between
-- @votes@ received and @seats@ won.
--
-- \[ G = \sqrt{\frac{1}{2}\sum_{i=1}^{n}(V_i-S_i)^2} \]
--
-- Both the @votes@ and @seats@ arguments should represent the integer number of votes or seats
-- won. However, Gallagher gives an index whose values range between 0 and 100. The argument lists
-- are converted to lists of the percentages of votes/seats, multiplied by 100.
--
-- \[   \sigma_v = \sum_{i=1}^{n}V_i\\
--      \sigma_s = \sum_{i=1}^{n}S_i\\
--      V_i := \frac{V_i}{\sigma_v} \times 100\\
--      S_i := \frac{S_i}{\sigma_s} \times 100\\
-- \]
gallagherIndex :: [Int] -> [Int] -> Double
gallagherIndex votes seats =
    let totalVotes = sum votes
        totalSeats = sum seats

        votesP = map (\vi -> fromIntegral vi / fromIntegral totalVotes * 100) votes
        seatsP = map (\si -> fromIntegral si / fromIntegral totalSeats * 100) seats

        sumOfSquares = sum $ zipWith (\vpi spi -> (vpi - spi) ** 2) votesP seatsP
     in sqrt $ sumOfSquares / 2

-- | Returns the list of indexes of competitors that achieved the threshold.
--
-- Given the threshold \(t\), and the tally list for \(k\) competitors \(V=\{v_0,\ldots,v_{k-1}\}\):
--
-- \[   \{ i | i\in\{0,\ldots,k-1\}, V_i \geq t^\prime \}\\
--      t^\prime = t \times \sigma\\
--      \sigma = \sum{V}\\
-- \]
aboveThreshold :: Double -> [Int] -> [Int]
aboveThreshold threshold votes =
    let totalVotes = sum votes
        thresholdVotes = fromIntegral totalVotes * threshold
     in findIndices (\vi -> fromIntegral vi >= thresholdVotes) votes