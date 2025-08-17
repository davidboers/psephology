module Psephology.ElectoralSystems.Condorcet (rankedPairs) where

import Data.List
import Data.Maybe
import qualified Data.Ord

import Psephology.Candidate
import Psephology.Condorcet (pairwiseMaj)
import Psephology.Counting
import Psephology.Voter

-- | [Ranked pairs](https://en.wikipedia.org/wiki/Ranked_pairs)
rankedPairs :: (Voter a) => [Candidate] -> [a] -> Int
rankedPairs candidates voters =
    fromJust $ findWinner $
    foldl
        ( \l (i, j, _) ->
            if createsParadox ((i, j) : l)
                then l
                else (i, j) : l
        )
        []
        $ sortOn (Data.Ord.Down . (\(_, _, maj) -> maj)) pairs
  where
    pairs =
        [ let maj = pairwiseMaj voters (candidates !! i) (candidates !! j)
           in if maj >= 0
                then (i, j, maj)
                else (j, i, -maj)
        | i <- [0 .. length candidates - 1]
        , j <- [0 .. length candidates - 1]
        , i /= j
        ]

    createsParadox :: [(Int, Int)] -> Bool
    createsParadox l' = isNothing (findWinner l')

    findWinner :: [(Int, Int)] -> Maybe Int
    findWinner l' =
        find (\c -> not $ any (\(_, j'') -> j'' == c) l') (map fst l')
