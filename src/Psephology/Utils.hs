{-# OPTIONS_GHC -Wno-type-defaults #-}

module Psephology.Utils (fullPreferentialPermutations, optionalPreferentialPermutations, integrate) where

factorial :: (Integral a) => a -> a
factorial n = product [1 .. n]

-- Simple trapezoid rule implementation
integrate :: (Double -> Double) -> Double -> Double -> Int -> Double
integrate f a b n =
    let h = (b - a) / fromIntegral n
        xs = [a + fromIntegral i * h | i <- [1 .. (n - 1)]]
        sumMiddle = sum (map f xs)
     in h * ((f a + f b) / 2 + sumMiddle)

-- Permutations

-- Returns the number of possible candidate orderings given a number of candidates @n@ without any truncated ballots.
fullPreferentialPermutations :: Int -> Int
fullPreferentialPermutations = factorial

-- Returns the number of possible candidate orderings given a number of candidates @n@ with truncated ballots.
optionalPreferentialPermutations :: Int -> Int
optionalPreferentialPermutations n = sum [factorial n `div` factorial (n - i) | i <- [1 .. n - 1]]
