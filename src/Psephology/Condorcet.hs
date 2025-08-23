{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Psephology.Condorcet
    ( numPreferOver
    , pairwiseMaj
    , condorcetWinner
    , condorcetMatrix
    , copelandScore
    , copelandSet
    , smithSet
    ) where

import Psephology.Candidate
import Psephology.Counting
import Psephology.Voter

import Data.List

-- | Returns the Condorcet winner if there is one.
condorcetWinner :: Voter a => [Candidate] -> [a] -> Maybe Int
condorcetWinner candidates voters =
    let winningScore = fromIntegral $ length candidates - 1
     in findIndex
            ((==) winningScore . copelandScore candidates voters)
            [0 .. length candidates - 1]

-- | @'numPreferOver' returns voters a b@ number of @voters@ that prefer @a@ over @b@.
numPreferOver :: Voter a => [a] -> Candidate -> Candidate -> Int
numPreferOver voters a b =
    head $ votes [a, b] voters

-- | @'pairwiseMaj' voters a b@ returns the number of voters that prefer @a@ minus the number of voters that prefer @b@.
pairwiseMaj :: Voter a => [a] -> Candidate -> Candidate -> Int
pairwiseMaj voters a b =
    let [pa, pb] = votes [a, b] voters
     in pa - pb

condorcetMatrix
    :: Voter a
    => ([a] -> Candidate -> Candidate -> Int)
    -> [Candidate]
    -> [a]
    -> [[Int]]
condorcetMatrix f candidates voters =
    [ [ f voters a b
      | b <- candidates
      ]
    | a <- candidates
    ]

{- | @'copelandScore' candidates voters c@ returns the number of other @candidates@ that @c@ beats in pairwise competitions. It's a 'Double' because
the [Copeland score](https://en.wikipedia.org/wiki/Copeland%27s_method#Computation) is 0.5 for tied pairs.
-}
copelandScore :: Voter a => [Candidate] -> [a] -> Int -> Double
copelandScore candidates voters c =
    sum
        [ copelandij $ pairwiseMaj voters (candidates !! c) (candidates !! b)
        | b <- [0 .. length candidates - 1]
        , c /= b
        ]

copelandij :: Int -> Double
copelandij m =
    case compare m 0 of
        LT -> 0
        EQ -> 0.5
        GT -> 1

-- | @'copelandSet' candidates voters@ returns a non-empty subset of @candidates@ that have the maximum Copeland score.
copelandSet :: Voter a => [Candidate] -> [a] -> [Int]
copelandSet candidates voters =
    let copelandScores = map (copelandScore candidates voters) [0 .. length candidates - 1]
        maxCopelandScore = maximum copelandScores
     in filter
            (\i -> maxCopelandScore == copelandScores !! i)
            [0 .. length candidates - 1]

-- | @'smithSet' candidates voters@ returns the smallest non-empty subset of @candidates@ that beat every candidate outside the set.
smithSet :: Voter a => [Candidate] -> [a] -> [Int]
smithSet candidates voters =
    foldl
        (smithSetStep candidates voters)
        (copelandSet candidates voters)
        [0 .. length candidates - 1]

smithSetStep :: Voter a => [Candidate] -> [a] -> [Int] -> Int -> [Int]
smithSetStep candidates voters set i
    | i `elem` set = set
    | any (\i' -> pairwiseMaj voters c (candidates !! i') >= 0) set = i : set
    | otherwise = set
  where
    c = candidates !! i
