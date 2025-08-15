{-# OPTIONS_GHC -Wno-type-defaults #-}

module Psephology.Utils (hare, droop, hagenbachBischoff, imperiali, majority, fullPreferentialPermutations, optionalPreferentialPermutations, integrate) where

import Psephology.Voter

factorial :: (Integral a) => a -> a
factorial n = product [1 .. n]

-- Simple trapezoid rule implementation
integrate :: (Double -> Double) -> Double -> Double -> Int -> Double
integrate f a b n =
    let h = (b - a) / fromIntegral n
        xs = [a + fromIntegral i * h | i <- [1 .. (n - 1)]]
        sumMiddle = sum (map f xs)
     in h * ((f a + f b) / 2 + sumMiddle)

-- Quotas

-- Returns the [Hare quota](https://en.wikipedia.org/wiki/Hare_quota)
hare :: Int -> Int -> Int
hare totalValidPoll seats =
    floor (fromIntegral totalValidPoll / fromIntegral seats)

-- Returns the [Droop quota](https://en.wikipedia.org/wiki/Droop_quota)
droop :: Int -> Int -> Int
droop totalValidPoll seats =
    floor (fromIntegral totalValidPoll / (fromIntegral seats + 1)) + 1

-- Returns the [Hagenbach-Bischoff quota](https://en.wikipedia.org/wiki/D%27Hondt_method)
hagenbachBischoff :: Int -> Int -> Int
hagenbachBischoff totalValidPoll seats =
    floor (fromIntegral totalValidPoll / (fromIntegral seats + 1))

-- Returns the [Imperiali quota](https://en.wikipedia.org/wiki/Imperiali_quota)
imperiali :: Int -> Int -> Int
imperiali totalValidPoll seats =
    floor (fromIntegral totalValidPoll / (fromIntegral seats + 2))

majority :: (Voter a) => [a] -> Int
majority voters = droop (length voters) 1

-- Permutations

-- Returns the number of possible candidate orderings given a number of candidates @n@ without any truncated ballots.
fullPreferentialPermutations :: Int -> Int
fullPreferentialPermutations = factorial

-- Returns the number of possible candidate orderings given a number of candidates @n@ with truncated ballots.
optionalPreferentialPermutations :: Int -> Int
optionalPreferentialPermutations n = sum [factorial n `div` factorial (n - i) | i <- [1 .. n - 1]]
