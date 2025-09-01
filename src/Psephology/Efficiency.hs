-- | This module is concerned with study of [social utility efficiency](https://en.wikipedia.org/wiki/Social_utility_efficiency).
module Psephology.Efficiency (
    -- * Efficiency ratios
    efficiency,

    -- * Helpers
    distance,
    phi,

    -- * Utility functions
    utilityC,
    utilityV,

    -- * Optimal candidates
    optimal,
    optimalPractical,
) where

import Data.List.Extras (argmax)

-- | Euclidean distance
distance :: [Double] -> [Double] -> Double
distance lhs rhs =
    sqrt $ sum $ zipWith (\v1 v2 -> (v1 - v2) ** 2) lhs rhs

{- | A [monotone decreasing function](https://en.wikipedia.org/wiki/Monotonic_function).

\[ \phi_i(d) = \|V_i\|-d \]

Literally:

\[ \phi(V_i,d) = distance((0,0),V_i)-d \]
-}
phi :: [Double] -> Double -> Double
phi xi d =
    let origin = replicate (length xi) 0
     in distance origin xi - d

{- | @'utilityV' voter p@ is the utility of @p@ to @voter@.

\[ U_i(p) = \phi_i\circ \|p-V_i\| \]
-}
utilityV :: [Double] -> [Double] -> Double
utilityV voter p =
    phi voter $ distance voter p

{- | @'utilityC' voters p@ the aggregated utility of @p@ for all @voters@.

\[ U_c(p) = \sum_{i=1}^N U_i(p) \]
-}
utilityC :: [[Double]] -> [Double] -> Double
utilityC voters p =
    sum $ map (`utilityV` p) voters

{- | @'optimal' detail voters@ is the (theoretical) optimal choice given @voters@.

\[ c^* = \arg \max_{(x,y)} U_c\circ (x,y) \]

To find possibles, the following step is used given \(x\) @details@.

\[ s = 10^{ -x } \]
-}
optimal :: Int -> [[Double]] -> [Double]
optimal detail voters =
    let step = 10 ** (-fromIntegral detail)
        makeBounds values = [minimum values, minimum values + step .. maximum values]
        candidates = mapM makeBounds voters
     in optimalPractical candidates voters

{- | @'optimalPractical' candidates voters@ is the optimal candidate in @candidates@.

\[ \underset{c \in C}{\arg\max} U_c(c) \]
-}
optimalPractical :: [[Double]] -> [[Double]] -> [Double]
optimalPractical candidates voters =
    argmax (utilityC voters) candidates

{- | @'efficiency' detail voters p@ returns the efficiency of elected candidate @p@ in
reflecting the (theoretical) optimal choice.

\[ SUE = \frac{U_c(p)}{U_c(c^*)} \]
-}
efficiency :: Int -> [[Double]] -> [Double] -> Double
efficiency detail voters p =
    utilityC voters p / utilityC voters (optimal detail voters)