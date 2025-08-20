module Psephology.ElectoralSystems.Borda (
    traditionalBordaWeight,
    dowdallWeight,
    icelandicWeight,
    bordaCount,
    weights,
    bordaTally,
    dowdallSystem,
    icelandicBorda,
) where

import Data.List.Extras (argmax)

import Psephology.Candidate
import Psephology.Voter

-- Borda count

-- | @'traditionalBordaWeight' n r@ returns @n@ - @r@ where @n@ is the total number of candidates and @r@ is the candidate rank. If @r@ is -1, returns 0.
traditionalBordaWeight :: Int -> Int -> Double
traditionalBordaWeight _ (-1) = 0
traditionalBordaWeight n r =
    fromIntegral $ n - r

-- | @'dowdallWeight' n r@ returns 1 / @r@ where @n@ is the total number of candidates and @r@ is the candidate rank. If @r@ is -1, returns 0.
dowdallWeight :: Int -> Int -> Double
dowdallWeight _ (-1) = 0
dowdallWeight _ r =
    1 / fromIntegral r

-- | @'icelandicWeight' n r@ returns (@n@ - @r@ - 1) / @n@ where @n@ is the total number of candidates and @r@ is the candidate rank.
icelandicWeight :: Int -> Int -> Double
icelandicWeight n r =
    fromIntegral (n - r + 1) / fromIntegral n

-- | @'weights' weightFormula candidates v@ returns a list of the weight given to each candidate by voter @v@.
weights :: (Voter a) => (Int -> Int -> Double) -> [Candidate] -> a -> [Double]
weights weightFormula candidates v =
    let n = length candidates
     in map (weightFormula n . rank candidates v) candidates

-- | @'bordaTally' weightFormula candidates voters@ returns the Borda tally for each candidate.
bordaTally :: (Voter a) => (Int -> Int -> Double) -> [Candidate] -> [a] -> [Double]
bordaTally weightFormula candidates =
    foldl (\t -> zipWith (+) t . weights weightFormula candidates) (replicate (length candidates) 0)

-- | @'bordaCountWFormula' weightFormula candidates voters@ returns the index of the candidate that wins a Borda count using @weightFormula@.
bordaCountWFormula :: (Voter a) => (Int -> Int -> Double) -> [Candidate] -> [a] -> Int
bordaCountWFormula weightFormula candidates voters =
    let points = bordaTally weightFormula candidates voters
     in argmax (points !!) [0 .. length candidates - 1]

-- | Shortcut for 'bordaCountWFormula'
bordaCount :: (Voter a) => [Candidate] -> [a] -> Int
bordaCount = bordaCountWFormula traditionalBordaWeight

-- | Shortcut for 'bordaCountWFormula'
dowdallSystem :: (Voter a) => [Candidate] -> [a] -> Int
dowdallSystem = bordaCountWFormula dowdallWeight

-- | Shortcut for 'bordaCountWFormula'
icelandicBorda :: (Voter a) => [Candidate] -> [a] -> Int
icelandicBorda = bordaCountWFormula icelandicWeight