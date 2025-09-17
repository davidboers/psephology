-- | This module contains algorithms for 10 Condorcet methods. Condorcet methods are a
-- family of voting systems defined by their adherence to the [Condorcet criterion](https://en.wikipedia.org/wiki/Condorcet_winner_criterion).
-- These voting systems will always elect the Condorcet winner, if one exists. The Condorcet
-- winner is the candidate preferred to every other in pairwise competitions.
--
-- In almost every case, the same winner is produced by all Condorcet methods. They diverge
-- only in how they deal with Condorcet paradoxes, or cases without a Condorcet winner. The
-- following is a brief description of how each system resolves a Condorcet paradox.
--
--     * __Nanson's method__ conducts a Borda count of the candidates, and iteratively excludes
--     the candidates that receive less than the average Borda score, until a single candidate
--     remains.
--     * __Baldwin's method__ is similar to Nanson's method, except it only excludes one
--     candidate at a time; that which has the lowest Borda score.
--     * The __Tideman alternative__ defaults to Instant Runoff Voting. First, all candidates not
--     in the [Smith set](https://en.wikipedia.org/wiki/Smith_set) are bulk excluded.
--     * The __Minimax__ method picks the candidate with the smallest pairwise margin of defeat
--     in the pairwise competition by which they lost the most.
--     * The __Copeland__ method, sometimes called __Llull's method__, is the simplest method.
--     It simply elects the candidate with the most pairwise wins against other candidates (highest
--     Copeland score).
--     * __Black's method__ defaults to a Borda count.
--     * The __Kemeny__ method, also called the __Kemeny-Young method__, selects the permutation
--     of candidates most representative of the results of the pairwise competitions. The winning
--     candidate is the candidate that ranks highest on this ordering. Computationally difficult
--     due to the analysis of every possible permutation, which increases by factorial.
--     * __Dodgson's method__ picks the winner by swapping voter preferences on individual ballots
--     until each candidate is a Condorcet winner. The candidate for whom the fewest swaps is
--     needed wins the election. The method is notable because it was invented by Lewis Carroll,
--     the author of Alice in Wonderland. Its extreme time-complexity means it is impractical
--     for large elections, making it a real Jabberwocky of a method (ba-dum-tiss).
--     * __Ranked pairs__ picks the winner by iteratively disregarding the pairwise competition
--     with the smallest margin of victory, until all Condorcet cycles have disappeared. It is
--     the method vulnerable to the least number of pathologies.
--     * The __Schulze__ method counts indirect victories using the transitive property.
--
-- The implementing algorithms in this module are not designed for optimal time efficiency.
--
-- This module is not to be confused with 'Psephology.Condorcet', which contains several functions
-- helpful for analyzing the compliance of a voting system with the Condorcet criterion.
module Psephology.ElectoralSystems.Condorcet
    ( nansonsMethod
    , baldwinsMethod
    , tidemanAlternative
    , minimax
    , copelandLlull
    , black
    , kemeny
    , dodgson
    , safeDodgson
    , rankedPairs
    , schulze

      -- * Helpers
    , kemenyOverallRanking
    , kemenyScore
    , dodgsonScore
    ) where

import Data.List
import Data.List.Extras (argmax)
import Data.Maybe
import Data.Function ((&))

import Psephology.Candidate
import Psephology.Condorcet
import Psephology.ElectoralSystems.Borda
import Psephology.ElectoralSystems.Runoff (instantRunoffVoting)
import Psephology.Voter
import Psephology.Utils (tallyWinner, replace)

-- | [Nanson](https://en.wikipedia.org/wiki/Nanson%27s_method#Nanson_method)
nansonsMethod :: Voter a => [Candidate] -> [a] -> Int
nansonsMethod = excludeByThreshold (\tally -> sum tally / fromIntegral (length tally))

-- | [Baldwin](https://en.wikipedia.org/wiki/Nanson%27s_method#Baldwin_method)
baldwinsMethod :: Voter a => [Candidate] -> [a] -> Int
baldwinsMethod = excludeByThreshold minimum

excludeByThreshold :: Voter a => ([Double] -> Double) -> [Candidate] -> [a] -> Int
excludeByThreshold _             [_]        _      = 0
excludeByThreshold thresholdFunc candidates voters =
    let tally = bordaTally traditionalBordaWeight candidates voters
        threshold = thresholdFunc tally
        keeping = findIndices (> threshold) tally
     in if null keeping -- Double check
            then tallyWinner tally
            else keeping !! excludeByThreshold thresholdFunc (map (candidates !!) keeping) voters

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

-- | \(\mathcal{O}(n!)\). [Kemeny](https://en.wikipedia.org/wiki/Kemeny_method). Computationally 
-- difficult with greater than ~10-12 candidates.
kemeny :: Voter a => [Candidate] -> [a] -> Int
kemeny candidates voters =
    head $ kemenyOverallRanking candidates voters

-- | \(\mathcal{O}(n!)\). Returns the ranking with the largest Kemeny score, as a list of candidate 
-- indexes. The winner of the Kemeny system is the candidate ranked first. Computationally difficult 
-- with greater than ~10-12 candidates.
kemenyOverallRanking :: Voter a => [Candidate] -> [a] -> [Int]
kemenyOverallRanking candidates voters =
    argmax (kemenyScore candidates voters) $
        permutations [0 .. length candidates - 1]

-- | @'kemenyScore' candidates voters ordering@ is the sum of the number of voters that prefer X over 
-- Y for every X \(\succ\) Y in @ordering@.
kemenyScore :: Voter a => [Candidate] -> [a] -> [Int] -> Int
kemenyScore candidates voters ordering =
    [ (candidates !! (ordering !! i), candidates !! (ordering !! j))
    | i <- [0     .. length ordering - 1]
    , j <- [i + 1 .. length ordering - 1]
    ]   & map (uncurry (numPreferOver voters))
        & sum

-- Dodgson

-- | [Dodgson](https://en.wikipedia.org/wiki/Dodgson%27s_method). Thanks to [Betzler et al.](https://www.sciencedirect.com/science/article/pii/S089054010900203X) for the algorithm.
--
--     +WARNING: This method has [exponential worst-case complexity](https://en.wikipedia.org/wiki/NP-hardness) and may become impractical with more than ~10 candidates.
dodgson :: Voter a => [Candidate] -> [a] -> Int
dodgson candidates voters =
    argminC (dodgsonScore candidates voters) candidates

-- | Avoids Dodgson's outrageous runtime, if possible, by:
--
-- * Catching Condorcet winners early, and only defaulting to the swap algorithm in case of a paradox.
-- * If a paradox is present, simplifying the count by limiting the search for each candidate Dodgson score to the running minimum.
safeDodgson :: Voter a => [Candidate] -> [a] -> Int
safeDodgson []         _      = -1
safeDodgson candidates voters =
    case condorcetWinner candidates voters of
        Just cw -> cw
        Nothing ->
            let (c0 : cs) = candidates
                s0 = fromJust (dodgsonScoreWorker Nothing candidates voters c0)
                step (bestS, bestI) (idx, ci) =
                    case dodgsonScoreWorker (Just bestS) candidates voters ci of
                        Just dscore | dscore < bestS -> (dscore, idx)
                        _                            -> (bestS, bestI)
                in snd $ foldl' step (s0, 0) (zip [1 .. length cs - 1] cs)

-- | @'dodgsonScore' candidates voters c@ is the minimum number of pairwise swaps in voting profile @voters@ needed to make @c@ a Condorcet winner.
-- Calculation of Dodgson score for a single candidate is itself [NP-hard](https://en.wikipedia.org/wiki/NP-hardness): runs in \(\mathcal{O}(2^k * nk+nm)\) where k is the Dodgson score,
-- n is the number of voters, and m is the number of candidates.
dodgsonScore :: Voter a => [Candidate] -> [a] -> Candidate -> Int
dodgsonScore candidates voters c =
    fromJust $ dodgsonScoreWorker Nothing candidates voters c

dodgsonScoreWorker :: Voter a => Maybe Int -> [Candidate] -> [a] -> Candidate -> Maybe Int
dodgsonScoreWorker k candidates voters c =
    swapTable voters d
    where
        switch :: Voter a => a -> Candidate -> Int
        switch vi cj
            | preference [c, cj] vi == 0 = 0
            | otherwise = rank candidates vi c - rank candidates vi cj

        best :: Voter a => [Candidate] -> a -> Candidate
        best s vi = s !! preference s vi

        d = map deficit cd
        deficit ci = pairwiseMaj voters ci c + 1
        p = length cd
        cd = filter (\ci -> ci /= c && deficit ci > 0) candidates

        lastD' :: [Int] -> [Candidate] -> [Int]
        lastD' d' s =
            [ if (cd !! i) `elem` s then max 0 (d' !! i - 1) else d' !! i
            | i <- [0 .. p - 1]
            ]

        swapTable :: Voter a => [a] -> [Int] -> Maybe Int
        swapTable [] d'
            | all (== 0) d' = Just 0
            | otherwise = Nothing
        swapTable (vi : vs) d' =
            let step sq s = do
                    tlast <- swapTable vs (lastD' d' s)
                    let new = tlast + switch vi (best s vi)
                    if new > fromMaybe new k
                        then Nothing
                        else minM sq $ Just new

                passable = [cd !! j | j <- [0 .. p - 1], d' !! j > 0, preference [cd !! j, c] vi == 0]
                subsets = filter (not . null) (subsequences passable)
             in foldl' step (swapTable vs d') subsets

        minM :: Maybe Int -> Maybe Int -> Maybe Int
        minM Nothing Nothing = Nothing
        minM Nothing rhs = rhs
        minM lhs Nothing = lhs
        minM (Just rhs) (Just lhs) = Just $ min rhs lhs

-- | [Ranked pairs](https://en.wikipedia.org/wiki/Ranked_pairs)
rankedPairs :: Voter a => [Candidate] -> [a] -> Int
rankedPairs candidates voters =
    [ case pairwiseMaj voters (candidates !! i) (candidates !! j) of
        maj | maj >= 0 -> (i, j, maj)
        maj            -> (j, i, -maj)
    | i <- [0 .. length candidates - 1]
    , j <- [0 .. length candidates - 1]
    , i /= j
    ] & sortOn (negate . (\(_, _, maj) -> maj))
      & foldl addUnlessParadox []
      & findWinner
      & fromJust
    where
        createsParadox :: [(Int, Int)] -> Bool
        createsParadox l' = isNothing (findWinner l')

        addUnlessParadox l (i, j, _)
            | createsParadox ((i, j) : l) = l
            | otherwise                   = (i, j) : l

        findWinner :: [(Int, Int)] -> Maybe Int
        findWinner l' =
            let (winners, losers) = unzip l'
             in fst <$> uncons (winners \\ losers)

-- Schulze method

-- | [Schulze](https://en.wikipedia.org/wiki/Schulze_method)
schulze :: Voter a => [Candidate] -> [a] -> Int
schulze candidates voters =
    [ (k, i, j)
    | k <- [0 .. length candidates - 1]
    , i <- [0 .. length candidates - 1]
    , i /= k
    , j <- [0 .. length candidates - 1]
    , j /= k && j /= i
    ] & foldl' schulzeStep (condorcetMatrix pairwiseMaj candidates voters)
      & beatpathWinner

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