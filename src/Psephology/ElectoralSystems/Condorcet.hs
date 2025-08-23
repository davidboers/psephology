module Psephology.ElectoralSystems.Condorcet
    ( nansonsMethod
    , baldwinsMethod
    , tidemanAlternative
    , minimax
    , copelandLlull
    , black
    , kemeny
    , rankedPairs
    , schulze

      -- * Helpers
    , kemenyOverallRanking
    , kemenyScore
    ) where

import Data.List
import Data.List.Extras (argmax)
import Data.Maybe
import qualified Data.Ord

import Psephology.Candidate
import Psephology.Condorcet
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

-- | [Minimax Condorcet](https://en.wikipedia.org/wiki/Minimax_Condorcet_method)
minimax :: Voter a => [Candidate] -> [a] -> Int
minimax candidates voters =
    argminC
        ( \x -> maximum $ map (\y -> max 0 $ numPreferOver voters y x) $ delete x candidates
        )
        candidates

-- | [Copeland-Llull](https://en.wikipedia.org/wiki/Copeland%27s_method)
copelandLlull :: Voter a => [Candidate] -> [a] -> Int
copelandLlull candidates voters =
    argmax (copelandScore candidates voters) [0 .. length candidates - 1]

-- | [Black](https://en.wikipedia.org/wiki/Black%27s_method). First argument is a Borda weight formula.
black :: Voter a => (Int -> Int -> Double) -> [Candidate] -> [a] -> Int
black weightFormula candidates voters =
    case condorcetWinner candidates voters of
        Just cw -> cw
        Nothing -> bordaCountWFormula weightFormula candidates voters

-- Kemeny

-- | \(\mathcal{O}(n!)\). [Kemeny](https://en.wikipedia.org/wiki/Kemeny_method). Computationally difficult with greater than ~10-12 candidates.
kemeny :: Voter a => [Candidate] -> [a] -> Int
kemeny candidates voters =
    head $ kemenyOverallRanking candidates voters

{- | \(\mathcal{O}(n!)\). Returns the ranking with the largest Kemeny score, as a list of candidate indexes.
The winner of the Kemeny system is the candidate ranked first. Computationally difficult with greater than ~10-12 candidates.
-}
kemenyOverallRanking :: Voter a => [Candidate] -> [a] -> [Int]
kemenyOverallRanking candidates voters =
    argmax (kemenyScore candidates voters) $
        permutations [0 .. length candidates - 1]

-- | @'kemenyScore' candidates voters ordering@ is the sum of the number of voters that prefer X over Y for every X \(\succ\) Y in @ordering@.
kemenyScore :: Voter a => [Candidate] -> [a] -> [Int] -> Int
kemenyScore candidates voters ordering =
    let pairs =
            [ (candidates !! (ordering !! i), candidates !! (ordering !! j))
            | i <- [0 .. length ordering - 1]
            , j <- [i + 1 .. length ordering - 1]
            ]
     in sum $ map (uncurry (numPreferOver voters)) pairs

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

-- Schulze method

-- | [Schulze](https://en.wikipedia.org/wiki/Schulze_method)
schulze :: Voter a => [Candidate] -> [a] -> Int
schulze candidates voters =
    beatpathWinner $
        foldl' schulzeStep (condorcetMatrix pairwiseMaj candidates voters) $
            [ (k, i, j)
            | k <- [0 .. length candidates - 1]
            , i <- [0 .. length candidates - 1]
            , i /= k
            , j <- [0 .. length candidates - 1]
            , j /= k && j /= i
            ]

schulzeStep :: [[Int]] -> (Int, Int, Int) -> [[Int]]
schulzeStep p (k, i, j) =
    let pij = max (p !! i !! j) (min (p !! i !! k) (p !! k !! j))
     in replace p (replace (p !! i) pij j) i

beatpathWinner :: [[Int]] -> Int
beatpathWinner [] = -1
beatpathWinner p =
    let n = length p
        strength i j = p !! i !! j
        isSchulzeWinner i =
            all (\j -> i == j || strength i j >= strength j i) [0 .. n - 1]
     in case find isSchulzeWinner [0 .. n - 1] of
            Just i -> i
            Nothing -> -1 -- deterministic fallback on full tie/cycle

replace :: [a] -> a -> Int -> [a]
replace [] _ _ = []
replace (_ : ls) x 0 = x : ls
replace (l : ls) x i = l : replace ls x (i - 1)