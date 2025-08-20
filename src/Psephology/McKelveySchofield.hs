-- | This sub-module is dedicated to the [McKelvey-Schofield chaos theorem](https://en.wikipedia.org/wiki/McKelvey%E2%80%93Schofield_chaos_theorem).
module Psephology.McKelveySchofield (findASpoiler, newMajority, spoilerPotential) where

import Psephology.Candidate (Candidate (Spacial))
import Psephology.Condorcet
import Psephology.Utils (integrate)
import Psephology.Voter (preference)

import Data.List
import Data.Maybe (fromMaybe)

{- | @'findASpoiler' p1 voters@ returns a new policy that is preferred by a majority of @voters@ to the current policy @p1@.
This solution does not produce the optimal spoiler, but only the first one found.

The McKelvey-Schofield theorem assumes at least 3 voters are present across at least 2 dimensions. If these conditions are
not met, function returns the status-quo candidate (p1).
-}
findASpoiler :: Candidate -> [[Double]] -> Candidate
findASpoiler p1 voters
    | length voters < 3 = p1
    | not (all ((>= 2) . length) voters) = p1
    | otherwise =
        let
            dimensions = minimum $ map length voters
            makeBounds d =
                let values = map (!! d) voters
                 in [minimum values, minimum values + 0.5 .. maximum values]
            candidates = map Spacial $ transpose $ map makeBounds [0 .. dimensions - 1]
            preferred p2 = pairwiseMaj voters p2 p1 > 0
         in
            fromMaybe p1 (find preferred candidates) -- fallback: should not occur if preconditions are met

-- | @'newMajority' p1 voters p2@ returns a list of the indexes of @voters@ that prefer @p2@ over @p1@.
newMajority :: Candidate -> [[Double]] -> Candidate -> [Int]
newMajority p1 voters p2 =
    filter (\i -> preference [p2, p1] (voters !! i) == 0) [0 .. length voters - 1]

{- | @'spoilerPotential' p1 voters@ returns the approximate proportion of theoretical policies (within the bounds, on each dimension,
of the existing @voters@) that would beat @p1@. The formula for a 2D space:

\[f(x, y) = \frac{\int_{a}^{b} \int_{c}^{d} s(x, y) \,dy \,dx}{(b - a)(d - c)}\]

\[s(x, y) =
    \begin{cases}
        1 & \sum_{n=1}^{i} \{ d((x,y),V_n) < d(p_1,V_n) : 1 , 0 \} > M \}\\
        0 & \text{otherwise}\\
    \end{cases}
\]

\[
M = floor(\frac{i}{2})
\]

Where \(V_i\) is a list of \(i\) voters and \(d\) is a Euclidean distance function.
-}
spoilerPotential :: Candidate -> [[Double]] -> Double
spoilerPotential p1 voters =
    let
        iter = 1000

        integrate' :: [[Double]] -> Double
        integrate' [] = 0
        integrate' (d : ds) =
            integrate f (minimum d) (maximum d) iter
          where
            f = integrate'' ds []

        integrate'' :: [[Double]] -> [Double] -> Double -> Double
        integrate'' [] ls l
            | pairwiseMaj voters p2 p1 > 0 = 1
            | otherwise = 0
          where
            p2 = Spacial $ reverse (l : ls)
        integrate'' (d : ds) ls l =
            integrate f (minimum d) (maximum d) iter
          where
            f = integrate'' ds (l : ls)

        totalSpace = product $ map (\d -> maximum d - minimum d) $ transpose voters
     in
        integrate' (transpose voters) / totalSpace