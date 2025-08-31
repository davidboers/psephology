{-# OPTIONS_GHC -Wno-type-defaults #-}

module Psephology.Utils (fullPreferentialPermutations, optionalPreferentialPermutations, integrate, median, medianI, split) where

import Data.List (elemIndex)

factorial :: Integral a => a -> a
factorial n = product [1 .. n]

-- Simple trapezoid rule implementation
integrate :: (Double -> Double) -> Double -> Double -> Int -> Double
integrate f a b n =
    let h = (b - a) / fromIntegral n
        xs = [a + fromIntegral i * h | i <- [1 .. (n - 1)]]
        sumMiddle = sum (map f xs)
     in h * ((f a + f b) / 2 + sumMiddle)

split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split a l =
    case elemIndex a l of
        Just i ->
            let (before, after) = splitAt i l
             in before : split a (drop 1 after)
        Nothing -> [l]

-- Permutations

-- Returns the number of possible candidate orderings given a number of candidates @n@ without any truncated ballots.
fullPreferentialPermutations :: Int -> Int
fullPreferentialPermutations = factorial

-- Returns the number of possible candidate orderings given a number of candidates @n@ with truncated ballots.
optionalPreferentialPermutations :: Int -> Int
optionalPreferentialPermutations n = sum [factorial n `div` factorial (n - i) | i <- [1 .. n - 1]]

-- Median

median :: Fractional a => [a] -> a
median [] = 0
median [a] = a
median [a, b] = (a + b) / 2
median l@(_ : _ : _ : _) = median $ tail $ init l

medianI :: Integral a => [a] -> a
medianI l = round $ median (map fromIntegral l :: Fractional b => [b])