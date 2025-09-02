-- | Given a tally list of \(k\) competitors (parties, alliances, constituencies, etc.)
-- \(V=\{v_1,\ldots,v_k\}\), the algorithm returns a new list \(S(k)\) indicating the number of
-- seats allocated to each competitor. The algorithm also takes in a number of seats to allocate
-- \(x\) and a divisor function \(f(n)\).
--
-- * Highest averages procedure
--
-- The following procedure is repeated until \(\sum_{i=1}^{k} S(i) = x\).
--
-- \[ \text{average}_i := \frac{v_i}{f \circ S(i)}\\
--    S(i) := S(i) +
--      \begin{cases}
--          1 & \text{if }i\text{ maximizes average}\\
--          0 & \text{otherwise}\\
--      \end{cases}
-- \]
--
-- \(S\) is preset to 0 for every \(i\).
--
-- * Properties
--
-- Note that for some divisor functions \(f(0) = 0\), meaning the averages blow up to infinity.
-- Because infinity is always the highest average, every party is destined to win a seat. This
-- applies to 'adams', 'dean', and 'huntingtonHill'.
--
-- \[ \text{average}_i = +\infty, \text{if} f \circ S(i)=0\\
--    S(i) \geq 1, \text{if }i \leq \min\{k,x\}\\
-- \]
--
-- If it is not desirable to give every competitor a free seat, an electoral threshold should be
-- set to exclude low-performing competitors.
--
-- The choice of divisor function determines whether smaller or larger parties will win marginal
-- seats. Smaller parties will benefit if the derivative of the divisor function is at least 1 at
-- any point on the relevant interval.
--
-- \[ \lim_{n \to x} f^\prime(n) \geq 1 \]
module Psephology.ProportionalRepresentation.HighestAverages
    ( -- * Entry point
      highestAverages
    , highestAveragesWithInit

      -- * Divisor formulae
    , adams
    , dean
    , dhondt
    , huntingtonHill
    , sainteLague
    , macanese
    ) where

import Data.Foldable (Foldable (foldl'))
import Data.List.Extras (argmax)

-- | @'highestAverages' divisor votes x@ returns the number of seats allocated to each competitor.
highestAverages :: (Int -> Double) -> [Int] -> Int -> [Int]
highestAverages divisor votes =
    highestAveragesWithInit (replicate (length votes) 0) divisor votes

-- | @'highestAveragesWithInit' init divisor votes x@ returns the number of seats allocated to each
-- competitor, with a custom initial seat count (@init@), allowing for calculation of the
-- [additional-member system](https://en.wikipedia.org/wiki/Additional-member_system).
highestAveragesWithInit :: [Int] -> (Int -> Double) -> [Int] -> Int -> [Int]
highestAveragesWithInit n0 divisor votes x =
    foldl' (\seats _ -> incrementAt divisor seats $ winnerIndex divisor votes seats) n0 [1 .. x]

incrementAt :: Num a => (Int -> Double) -> [a] -> Int -> [a]
incrementAt _ [] _ = []
incrementAt divisor (x : xs) i
    | i == 0 = x + 1 : xs
    | otherwise = x : incrementAt divisor xs (i - 1)

winnerIndex :: (Int -> Double) -> [Int] -> [Int] -> Int
winnerIndex divisor votes seats =
    argmax (\i -> fromIntegral (votes !! i) / divisor (seats !! i)) [0 .. length votes - 1]

-- | Rounding up of seats.
--
-- \[ f(n) = n \]
--
-- Benefits lower-vote-getters because \(f(0) = 0\).
adams :: Int -> Double
adams = fromIntegral

-- | Harmonic progression.
--
-- \[ f(n) = \frac{2}{\frac{1}{n} + \frac{1}{n+1}} \]
--
-- Complex \(f^\prime(n) = 1 + \frac{1}{4(n+\frac{1}{2})^2}\).
dean :: Int -> Double
dean n = 2 / ((1 / fromIntegral n) + (1 / (fromIntegral n + 1)))

-- | Rounding down of seats.
--
-- \[ f(n) = n + 1 \]
--
-- Benefits higher-vote-getters because \(f^\prime(n) = 1\).
dhondt :: Int -> Double
dhondt n = fromIntegral $ n + 1

-- | Geometric progression.
--
-- \[ f(n) = \sqrt{n(n+1)} \]
--
-- Benefits lower-vote-getters because \(f(0) = 0\) and \(f^\prime(n) = \frac{2n+1}{2\sqrt{n(n+1)}}\).
huntingtonHill :: Int -> Double
huntingtonHill n =
    sqrt $ fromIntegral n * (fromIntegral n + 1)

-- | Arithmetic progression.
--
-- \[ f(n) = 2n+1 \]
--
-- Benefits lower-vote-getters because \(f^\prime(n) = 2\).
sainteLague :: Int -> Double
sainteLague n = fromIntegral $ 2 * n + 1

-- | Polynomial progression.
--
-- \[ f(n) = 2^n \]
--
-- Benefits lower-vote-getters because \(f^\prime(n) = 2^n\text{ln}2\).
macanese :: Int -> Double
macanese n = 2 ** fromIntegral n