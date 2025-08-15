module Psephology.Quotas (hare, droop, hagenbachBischoff, imperiali, majority) where

import Psephology.Voter

-- Returns the [Hare quota](https://en.wikipedia.org/wiki/Hare_quota)
hare :: Int -> Int -> Int
hare totalValidPoll seats =
    floor (fromIntegral totalValidPoll / fromIntegral seats)

-- Returns the [Droop quota](https://en.wikipedia.org/wiki/Droop_quota)
droop :: Int -> Int -> Int
droop totalValidPoll seats =
    floor (fromIntegral totalValidPoll / (fromIntegral seats + 1)) + 1

-- Returns the [Hagenbach-Bischoff quota](https://en.wikipedia.org/wiki/D%27Hondt_method)
hagenbachBischoff :: Int -> Int -> Int
hagenbachBischoff totalValidPoll seats =
    floor (fromIntegral totalValidPoll / (fromIntegral seats + 1))

-- Returns the [Imperiali quota](https://en.wikipedia.org/wiki/Imperiali_quota)
imperiali :: Int -> Int -> Int
imperiali totalValidPoll seats =
    floor (fromIntegral totalValidPoll / (fromIntegral seats + 2))

majority :: (Voter a) => [a] -> Int
majority voters = droop (length voters) 1