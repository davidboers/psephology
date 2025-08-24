{- | Named after the Marquis de Borda, a contemporary of Condorcet, the [Borda count](https://en.wikipedia.org/wiki/Borda_count) is
a simple, pragmatic voting method. It has some technical advantages over other ordinal methods, such as being easier to count. It
also has a high probability of choosing the Condorcet winner, empirically the highest of any non-Condorcet ordinal voting method.
However, it is often derided for its vulnerability to manipulation. Although less prone to *exhibiting* pathological behavior, 
its vulnerable to a greater *number* of them than Condorcet methods, Instant Runoff Voting, or plurality systems.

The different Borda variants are procedurally identical. They differ only in the formula used to determine the number of points
awarded for each ranking. The 'bordaCountWFormula' function is the general algorithm entry point, and can be specialized by passing
'traditionalBordaWeight', 'dowdallWeight', or 'icelandicWeight' as its first argument. There also exist shortcut aliases for each.

+--------------------------+------------------+-----------------------+-------------------------------------+
| Weight method            | Shortcut alias   | Weight formula        | Usage/notes                         |
+==========================+==================+=======================+=====================================+
| 'traditionalBordaWeight' | 'bordaCount'     | \[ n-r \]             | Borda's original proposal           |
+--------------------------+------------------+-----------------------+-------------------------------------+
| 'dowdallWeight'          | 'dowdallSystem'  | \[ \frac{1}{r} \]     | Used in Nauru                       |
+--------------------------+------------------+-----------------------+-------------------------------------+
| 'icelandicWeight'        | 'icelandicBorda' | \[ \frac{n-r-1}{n} \] | Used to order Icelandic party lists |
+--------------------------+------------------+-----------------------+-------------------------------------+

where @n@ is the number of candidates (in total, not just that are ranked by the voter) and @r@ is the numerical rank (1..).

@
    bordaCountWFormula traditionalBordaWeight candidates voters = bordaCount candidates voters
    bordaCountWFormula dowdallWeight candidates voters          = dowdallSystem candidates voters
    bordaCountWFormula icelandicWeight candidates voters        = icelandicBorda candidates voters
@
-}
module Psephology.ElectoralSystems.Borda
    ( -- * Weight formulae
      traditionalBordaWeight
    , dowdallWeight
    , icelandicWeight

      -- * Counting
    , weights
    , bordaTally

      -- * Entry points
    , bordaCountWFormula
    , bordaCount
    , dowdallSystem
    , icelandicBorda
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
weights :: Voter a => (Int -> Int -> Double) -> [Candidate] -> a -> [Double]
weights weightFormula candidates v =
    let n = length candidates
     in map (weightFormula n . rank candidates v) candidates

-- | @'bordaTally' weightFormula candidates voters@ returns the Borda tally for each candidate.
bordaTally
    :: Voter a => (Int -> Int -> Double) -> [Candidate] -> [a] -> [Double]
bordaTally weightFormula candidates =
    foldl
        (\t -> zipWith (+) t . weights weightFormula candidates)
        (replicate (length candidates) 0)

-- | @'bordaCountWFormula' weightFormula candidates voters@ returns the index of the candidate that wins a Borda count using @weightFormula@.
bordaCountWFormula
    :: Voter a => (Int -> Int -> Double) -> [Candidate] -> [a] -> Int
bordaCountWFormula weightFormula candidates voters =
    let points = bordaTally weightFormula candidates voters
     in argmax (points !!) [0 .. length candidates - 1]

-- | Shortcut for 'bordaCountWFormula'
bordaCount :: Voter a => [Candidate] -> [a] -> Int
bordaCount = bordaCountWFormula traditionalBordaWeight

-- | Shortcut for 'bordaCountWFormula'
dowdallSystem :: Voter a => [Candidate] -> [a] -> Int
dowdallSystem = bordaCountWFormula dowdallWeight

-- | Shortcut for 'bordaCountWFormula'
icelandicBorda :: Voter a => [Candidate] -> [a] -> Int
icelandicBorda = bordaCountWFormula icelandicWeight