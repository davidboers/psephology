{-# OPTIONS_GHC -Wno-type-defaults #-}

module Psephology.Utils (fullPreferentialPermutations, optionalPreferentialPermutations, factorial, indices, integrate, median, medianI, split, incrementAt, tallyWinner, tallyLoser, normalize, zipWith2D, replace, update) where

import Data.List.Extras (argmax, argmin)
import Data.List (elemIndex)

factorial :: Integral a => a -> a
factorial 0 = 1
factorial n = n * (n - 1)

{-# INLINE indices #-}
indices :: [a] -> [Int]
indices l = [0..length l - 1]

normalize :: [Double] -> [Double]
normalize x =
    let total = sum x
     in map (/ total) x

{-# INLINE zipWith2D #-}
zipWith2D :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
zipWith2D f =
    zipWith (zipWith f)

replace :: [a] -> a -> Int -> [a]
replace xs x = update xs (const x)

update :: [a] -> (a -> a) -> Int -> [a]
update [] _ _ = []
update (x : xs) f 0 = f x : xs
update (x : xs) f i = x : update xs f (i - 1)

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

incrementAt :: Num a => [a] -> Int -> [a]
incrementAt [] _ = []
incrementAt (x : xs) i
    | i == 0 = x + 1 : xs
    | otherwise = x : incrementAt xs (i - 1)

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
median l@(_ : _ : _ : _) = median $ drop 1 $ init l

medianI :: Integral a => [a] -> a
medianI l = round $ median (map fromIntegral l :: Fractional b => [b])

-- Tally winner

tallyWinner :: Ord a => [a] -> Int
tallyWinner tally = 
    argmax (tally !!) (indices tally)

tallyLoser :: Ord a => [a] -> Int
tallyLoser tally =
    argmin (tally !!) (indices tally)