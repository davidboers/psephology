module Psephology.ElectoralSystems.Condorcet (nansonsMethod, baldwinsMethod, tidemanAlternative, copelandLlull, rankedPairs) where

import Data.List
import Data.List.Extras (argmax)
import Data.Maybe
import qualified Data.Ord

import Psephology.Candidate
import Psephology.Condorcet (copelandScore, pairwiseMaj, smithSet)
import Psephology.ElectoralSystems.Borda
import Psephology.ElectoralSystems.Runoff (instantRunoffVoting)
import Psephology.Voter

-- | [Nanson](https://en.wikipedia.org/wiki/Nanson%27s_method#Nanson_method)
nansonsMethod :: Voter a => [Candidate] -> [a] -> Int
nansonsMethod [_] _ = 0
nansonsMethod candidates voters =
    let tally = bordaTally traditionalBordaWeight candidates voters
        threshold = sum tally / fromIntegral (length candidates)
        keeping = filter (\i -> (tally !! i) > threshold) [0 .. length candidates - 1]
     in if null keeping -- Double check
            then argmax (tally !!) [0 .. length candidates - 1]
            else keeping !! nansonsMethod (map (candidates !!) keeping) voters

-- | [Baldwin](https://en.wikipedia.org/wiki/Nanson%27s_method#Baldwin_method)
baldwinsMethod :: Voter a => [Candidate] -> [a] -> Int
baldwinsMethod [_] _ = 0
baldwinsMethod candidates voters =
    let tally = bordaTally traditionalBordaWeight candidates voters
        threshold = minimum tally
        keeping = filter (\i -> (tally !! i) /= threshold) [0 .. length candidates - 1]
     in if null keeping -- Double check
            then argmax (tally !!) [0 .. length candidates - 1]
            else keeping !! baldwinsMethod (map (candidates !!) keeping) voters

-- | [Tideman alternative](https://en.wikipedia.org/wiki/Tideman_alternative_method)
tidemanAlternative :: Voter a => [Candidate] -> [a] -> Int
tidemanAlternative candidates voters =
    let smith = smithSet candidates voters
     in smith !! instantRunoffVoting (map (candidates !!) smith) voters

-- | [Copeland-Llull](https://en.wikipedia.org/wiki/Copeland%27s_method)
copelandLlull :: Voter a => [Candidate] -> [a] -> Int
copelandLlull candidates voters =
    argmax (copelandScore candidates voters) [0 .. length candidates - 1]

-- | [Ranked pairs](https://en.wikipedia.org/wiki/Ranked_pairs)
rankedPairs :: Voter a => [Candidate] -> [a] -> Int
rankedPairs candidates voters =
    fromJust
        $ findWinner
        $ foldl
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
