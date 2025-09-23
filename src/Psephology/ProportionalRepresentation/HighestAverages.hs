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
-- seats. Smaller parties will benefit if the derivative of the divisor function is greater than 1 
-- at any point on the relevant interval.
--
-- \[ \lim_{n \to x} f^\prime(n) > 1 \]
{-# LANGUAGE CPP #-}
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

#if __GLASGOW_HASKELL__ < 910
import Data.List (foldl')
#endif

import Psephology.ProportionalRepresentation (checkRelative)

import Psephology.Utils (incrementAt, tallyWinner)

-- | @'highestAverages' divisor votes x@ returns the number of seats allocated to each competitor.
{-# NOINLINE highestAverages #-}
highestAverages :: (Int -> Double) -> [Int] -> Int -> [Int]
highestAverages divisor votes =
    highestAveragesWithInit (map (const 0) votes) divisor votes

{-# RULES 
"Adams obey checkRelative" forall votes numSeats.
    checkRelative votes (highestAverages adams votes numSeats) = True -- Does not apply

"Dean obey checkRelative" forall votes numSeats.
    checkRelative votes (highestAverages dean votes numSeats) = True -- Does not apply

"D'Hondt obey checkRelative" forall votes numSeats. 
    checkRelative votes (highestAverages dhondt votes numSeats) = True

"Huntington-Hill obey checkRelative" forall votes numSeats. 
    checkRelative votes (highestAverages huntingtonHill votes numSeats) = True -- Does not apply

"Saintë-Lague obey checkRelative" forall votes numSeats. 
    checkRelative votes (highestAverages sainteLague votes numSeats) = True

"Macanese obey checkRelative" forall votes numSeats. 
    checkRelative votes (highestAverages macanese votes numSeats) = True
#-}

-- | @'highestAveragesWithInit' init divisor votes x@ returns the number of seats allocated to each
-- competitor, with a custom initial seat count (@init@), allowing for calculation of the
-- [additional-member system](https://en.wikipedia.org/wiki/Additional-member_system).
highestAveragesWithInit :: [Int] -> (Int -> Double) -> [Int] -> Int -> [Int]
highestAveragesWithInit n0 divisor votes x =
    foldl' (\seats _ -> incrementAt seats $ winnerIndex divisor votes seats) n0 [1 .. x]

winnerIndex :: (Int -> Double) -> [Int] -> [Int] -> Int
winnerIndex divisor votes seats =
    tallyWinner $ zipWith (\vi si -> fromIntegral vi / divisor si) votes seats

-- | Rounding up of seats.
--
-- \[ f(n) = n \]
--
-- Benefits lower-vote-getters because \(f(0) = 0\).
{-# INLINE [1] adams #-}
adams :: Int -> Double
adams = fromIntegral

-- | Harmonic progression.
--
-- \[ f(n) = \frac{2n(n+1)}{2n+1} \]
--
-- Complex \(f^\prime(n) = \frac{x^2 + x + \frac{1}{2}}{x^2 + x + \frac{1}{4}}\).
{-# INLINE [1] dean #-}
dean :: Int -> Double
dean n = fromIntegral (2 * n * (n + 1)) / fromIntegral (2 * n + 1) 

-- | Rounding down of seats.
--
-- \[ f(n) = n + 1 \]
--
-- Benefits higher-vote-getters because \(f^\prime(n) = 1\).
{-# INLINE [1] dhondt #-}
dhondt :: Int -> Double
dhondt n = fromIntegral $ n + 1

-- | Geometric progression.
--
-- \[ f(n) = \sqrt{n(n+1)} \]
--
-- Benefits lower-vote-getters because \(f(0) = 0\) and \(f^\prime(n) = \frac{2n+1}{2\sqrt{n(n+1)}}\).
{-# INLINE [1] huntingtonHill #-}
huntingtonHill :: Int -> Double
huntingtonHill n =
    sqrt $ fromIntegral n * (fromIntegral n + 1)

-- | Arithmetic progression.
--
-- \[ f(n) = 2n+1 \]
--
-- Benefits lower-vote-getters because \(f^\prime(n) = 2\).
{-# INLINE [1] sainteLague #-}
sainteLague :: Int -> Double
sainteLague n = fromIntegral $ 2 * n + 1

-- | Polynomial progression.
--
-- \[ f(n) = 2^n \]
--
-- Benefits lower-vote-getters because \(f^\prime(n) = 2^n\text{ln}2\).
{-# INLINE [1] macanese #-}
macanese :: Int -> Double
macanese n = 2 ** fromIntegral n