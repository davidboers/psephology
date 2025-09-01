-- | [Highest averages method](https://en.wikipedia.org/wiki/Highest_averages_method)
module Psephology.ProportionalRepresentation.HighestAverages
    ( -- * Entry point
      highestAverages

      -- * Divisor formulae
    , adams
    , dean
    , dhondt
    , huntingtonHill
    , sainteLague
    , macanese
    ) where

import Data.Foldable (Foldable (foldl'))
import Data.List.Extras (argmax, argmin)

highestAverages :: (Int -> Double) -> [Double] -> Int -> [Int]
highestAverages divisor votes x =
    foldl' (\seats _ -> incrementAt divisor seats $ winnerIndex divisor votes seats) (replicate (length votes) 0) [0 .. x - 1]

incrementAt :: Num a => (Int -> Double) -> [a] -> Int -> [a]
incrementAt _ [] _ = []
incrementAt divisor (x : xs) i
    | i == 0 = x + 1 : xs
    | otherwise = x : incrementAt divisor xs (i - 1)

winnerIndex :: (Int -> Double) -> [Double] -> [Int] -> Int
winnerIndex divisor votes seats =
    argmax (\i -> votes !! i / divisor (seats !! i)) [0 .. length votes - 1]

-- | Rounding up of seats.
--
-- \[ A_n = n
--    A_0 = \infty
-- \]
--
-- Benefits lower-vote-getters because \(f(0) = \infty\).
adams :: Int -> Double
adams = n

-- | Harmonic progression.
--
-- \[ A_n = \frac{2}{\frac{1}{n} + \frac{1}{n+1}}
--    A_0 = \infty
-- \]
--
-- Complex \(f\prime(n) = 1 + \frac{1}{4(n+\frac{1}{2})^2}\).
dean :: Int -> Double
dean n = 2 / ((1 / fromIntegral n) + (1 / (fromIntegral n + 1)))

-- | Rounding down of seats.
--
-- \[ A_n = n + 1 \]
--
-- Benefits higher-vote-getters because \(f\prime(n) = 1\).
dhondt :: Int -> Double
dhondt n = fromIntegral $ n + 1

-- | Geometric progression.
--
-- \[ A_n = \sqrt{n(n+1)}
--    A_0 = \infty
-- \]
--
-- Benefits lower-vote-getters because \(f(0) = \infty\) and \(f\prime(n) = \frac{2n+1}{2\sqrt{n(n+1)}}\).
huntingtonHill :: Int -> Double
huntingtonHill n =
    sqrt $ fromIntegral n * (fromIntegral n + 1)

-- | Arithmetic progression.
--
-- \[ A_n = 2n+1 \]
--
-- Benefits lower-vote-getters because \(f\prime(n) = 2\).
sainteLague :: Int -> Double
sainteLague n = fromIntegral $ 2 * n + 1

-- | Polynomial progression.
--
-- \[ A_n = 2^n \]
--
-- Benefits lower-vote-getters because \(f\prime(n) = 2^n\text{ln}2\).
macanese :: Int -> Double
macanese n = 2 ** fromIntegral n