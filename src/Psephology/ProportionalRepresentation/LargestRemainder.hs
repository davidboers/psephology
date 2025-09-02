-- | A placeholder module for the [largest remainder method](https://en.wikipedia.org/wiki/Quota_method).
module Psephology.ProportionalRepresentation.LargestRemainder where

import Data.List (sortOn)
import qualified Data.Ord

largestRemainder :: (Int -> Int -> Int) -> [Int] -> Int -> [Int]
largestRemainder quotaFunction votes x =
    let totalVotes = sum votes
        quota = quotaFunction totalVotes x
        initials = map (`div` quota) votes
        emptySeats = x - sum initials
        remainders = map (`mod` quota) votes
        remainderIndexes = take emptySeats $ cycle $ sortOn (Data.Ord.Down . (remainders !!)) [0 .. length votes - 1]
     in [initials !! i + length (filter (== i) remainderIndexes) | i <- [0 .. length votes - 1]]