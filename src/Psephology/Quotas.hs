{-# OPTIONS_GHC -Wno-type-defaults #-}

{- | This module consists of the functions commonly used to calculate quotas in proportional
representation elections. Quotas are positive integers. These functions can also be used to
determine majority marks in single-winner elections such as the two-round system. For example,
the number of votes needed to win an absolute majority is the Droop quota.
-}
module Psephology.Quotas (hare, droop, hagenbachBischoff, imperiali, majority) where

import Psephology.Voter

{- | Returns the [Hare quota](https://en.wikipedia.org/wiki/Hare_quota).

\[ Q_h = \frac{\text{total valid poll}}{\text{seats}} \]
-}
hare :: Int -> Int -> Int
hare totalValidPoll seats =
    floor (fromIntegral totalValidPoll / fromIntegral seats)

{- | Returns the [Droop quota](https://en.wikipedia.org/wiki/Droop_quota).

\[ Q_d = \lfloor \frac{\text{total valid poll}}{\text{seats}+1} \rfloor +1 \]
-}
droop :: Int -> Int -> Int
droop totalValidPoll seats =
    floor (fromIntegral totalValidPoll / (fromIntegral seats + 1)) + 1

{- | Returns the [Hagenbach-Bischoff quota](https://en.wikipedia.org/wiki/D%27Hondt_method).

\[ Q_hb = \lfloor \frac{\text{total valid poll}}{\text{seats}+1} \rfloor \]
-}
hagenbachBischoff :: Int -> Int -> Int
hagenbachBischoff totalValidPoll seats =
    floor (fromIntegral totalValidPoll / (fromIntegral seats + 1))

{- | Returns the [Imperiali quota](https://en.wikipedia.org/wiki/Imperiali_quota).

\[ Q_I = \lfloor \frac{\text{total valid poll}}{\text{seats}+2} \rfloor \]
-}
imperiali :: Int -> Int -> Int
imperiali totalValidPoll seats =
    floor (fromIntegral totalValidPoll / (fromIntegral seats + 2))

{- | The absolute majority of voters. Equal to the Droop quota for 1 election.

> majority voters = droop (length voters) 1
-}
majority :: Voter a => [a] -> Int
majority voters = droop (length voters) 1