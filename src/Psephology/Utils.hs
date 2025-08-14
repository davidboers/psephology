module Psephology.Utils where

import Psephology.Voter

-- Returns the Droop quota
droop :: Int -> Int -> Int
droop totalValidPoll seats =
    floor (fromIntegral totalValidPoll / (fromIntegral seats + 1)) + 1

majority :: (Voter a) => [a] -> Int
majority voters = droop (length voters) 1