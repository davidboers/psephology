-- | This sub-module is dedicated to the [McKelvey-Schofield chaos theorem](https://en.wikipedia.org/wiki/McKelvey%E2%80%93Schofield_chaos_theorem).
module Psephology.McKelveySchofield (
    utility,
    spoilerPotential,
    -- Single spoilers
    findASpoiler,
    newMajority,
    -- Spoiler paths
    thetaPath,
) where

import Psephology.Candidate (Candidate (Spacial))
import Psephology.Condorcet
import Psephology.Utils (integrate)
import Psephology.Voter (dist, preference)

import Data.List
import Data.List.Extras (argmax)
import Data.Maybe (fromMaybe)

-- | Returns a point in a bounded space that fulfils the requirements of a function.
pointInSpace :: ([Double] -> Bool) -> [(Double, Double)] -> Maybe [Double]
pointInSpace f =
    find f . candidates

bounds :: [[Double]] -> [(Double, Double)]
bounds = map makeBounds . transpose
  where
    makeBounds values = (minimum values, maximum values)

candidates :: [(Double, Double)] -> [[Double]]
candidates bs =
    let makeCandidates (mn, mx) = [mn, mn + 0.5 .. mx]
     in transpose $ map makeCandidates bs

{- | @'findASpoiler' p1 voters@ returns a new policy that is preferred by a majority of @voters@ to the current policy @p1@.
This solution does not produce the optimal spoiler, but only the first one found.

The McKelvey-Schofield theorem assumes at least 3 voters are present across at least 2 dimensions. If these conditions are
not met, function returns the status-quo candidate (p1).
-}
findASpoiler :: [Double] -> [[Double]] -> [Double]
findASpoiler p1 voters
    | length voters < 3 = p1
    | not (all ((>= 2) . length) voters) = p1
    | otherwise =
        let preferred p2 = pairwiseMaj voters (Spacial p2) (Spacial p1) > 0
         in fromMaybe p1 (pointInSpace preferred (bounds voters)) -- fallback: should not occur if preconditions are met

{- | @'thetaPath' p1 voters p2@ returns a list of points from @p1@ (exclusive) to @p2@ (inclusive) that iteratively spoil the
election, leading to @p2@. The algorithm is conjectured to provide the optimal solution.

Undefined behavior: Inputting non-preconditioned @voters@; inputting @p2@ that is an immediate spoiler of @p1@.
-}
thetaPath :: [Double] -> [[Double]] -> [Double] -> [[Double]]
thetaPath p1 voters p2 =
    let bounds_v = bounds voters
        immediateSpoiler px =
            (pairwiseMaj voters (Spacial px) (Spacial p1) > 0) && 
            (pairwiseMaj voters (Spacial p2) (Spacial px) > 0)
     in case pointInSpace immediateSpoiler bounds_v of
            Just ti -> [ti, p2]
            Nothing ->
                let ti = argmax (dist p1 . Spacial) (candidates bounds_v) -- Produce the most extreme possible spoiler.
                 in ti : thetaPath ti voters p2

{- | @'utility' xi x@ returns voter the utility of policy @x@ to voter @xi@, given McKelvey's original definition of the utility
function:

\[U_i(x) = \phi_i\circ \|x-x_i\|\]

A more Haskellic way of putting it:

\[U(x_i,x) = \phi\circ \|x-x_i\|\]

\(\|\|\) is the Euclidean distance between the two points, and \(\phi\), which is defined by McKelvey as "any monotone decreasing
function":

\[\phi(d) = 1-d\]

This module actually makes minimal usage of this function, finding other ways to measure utility/distance. This function is provided
for the purpose of completeness.
-}
utility :: [Double] -> [Double] -> Double
utility xi x =
    let phi d = 1 - d
     in phi $ sqrt $ sum $ zipWith (\v1 v2 -> (v1 - v2) ** 2) x xi

-- | @'newMajority' p1 voters p2@ returns a list of the indexes of @voters@ that prefer @p2@ over @p1@.
newMajority :: [Double] -> [[Double]] -> [Double] -> [Int]
newMajority p1 voters p2 =
    filter (\i -> preference [Spacial p2, Spacial p1] (voters !! i) == 0) [0 .. length voters - 1]

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
spoilerPotential :: [Double] -> [[Double]] -> Double
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
            | pairwiseMaj voters (Spacial p2) (Spacial p1) > 0 = 1
            | otherwise = 0
          where
            p2 = reverse (l : ls)
        integrate'' (d : ds) ls l =
            integrate f (minimum d) (maximum d) iter
          where
            f = integrate'' ds (l : ls)

        totalSpace = product $ map (\d -> maximum d - minimum d) $ transpose voters
     in
        integrate' (transpose voters) / totalSpace