{-# OPTIONS_GHC -Wno-name-shadowing #-}
-- | The BBC's patented seat projector. [Read more](https://en.wikipedia.org/wiki/Swingometer).
module Psephology.Polling.Swingometer (swing, swingSeat, seatProjection, makeDeltas) where

import Data.List.Extras.Argmax (argmax)

-- | Computes the projected results for multiple seats.
swing :: [Double] -> [[Int]] -> [[Int]]
swing deltas =
    map (swingSeat deltas)

-- | Computes the projected results for a single contest.
swingSeat :: [Double] -> [Int] -> [Int]
swingSeat deltas lastResultSeat =
    map round $ zipWith (+) deltas $ normalize $ map fromIntegral lastResultSeat

-- | Returns the projected seat count for each party.
seatProjection :: [Double] -> [[Int]] -> [Int]
seatProjection deltas lastResults =
    counts $ map winner $ swing deltas lastResults

-- | @'makeDeltas' last now@ returns the delta vector between @now@ and @last@. Input vectors are
-- normalized, so it doesn't matter if you input raw votes or vote percentages.
-- 
-- \[
--      |N| = |L|\\
--      N^{\prime}_i = \frac{N_i}{\sum{N}}\\
--      L^{\prime}_i = \frac{L_i}{\sum{L}}\\
--      \Delta = N^{\prime} - L^{\prime}
-- \]
makeDeltas :: [Double] -> [Double] -> [Double]
makeDeltas last now =
    zipWith (-) (normalize now) (normalize last)

normalize :: [Double] -> [Double]
normalize x =
    let total = sum x
     in map (/ total) x

counts :: [Int] -> [Int]
counts is =
    [ length $ filter (== i) is
    | i <- [0..maximum is]
    ]

winner :: Ord a => [a] -> Int
winner ns = argmax (ns !!) [0..length ns-1]