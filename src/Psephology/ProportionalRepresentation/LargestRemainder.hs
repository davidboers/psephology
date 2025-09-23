-- | A placeholder module for the [largest remainder method](https://en.wikipedia.org/wiki/Quota_method).
module Psephology.ProportionalRepresentation.LargestRemainder where

import Data.List (sortOn)

import Psephology.Utils (indices)

-- | @'largestRemainder' quotaFunction votes x@ returns the number of seats allocated to each competitor.
{-# NOINLINE largestRemainder #-}
largestRemainder :: (Int -> Int -> Int) -> [Int] -> Int -> [Int]
largestRemainder _             votes 0 = map (const 0) votes -- Escape divide by 0 errors
largestRemainder quotaFunction votes x =
    let totalVotes = sum votes
        quota = max 1 $ quotaFunction totalVotes x -- Temporary solution
        initials = map (`div` quota) votes
        emptySeats = x - sum initials
        remainders = map (`mod` quota) votes
        remainderIndexes = take emptySeats $ cycle $ sortOn (negate . (remainders !!)) (indices votes)
     in [initials !! i + length (filter (== i) remainderIndexes) | i <- indices votes]

{-# RULES
"If x=0" forall (quotaFunction :: Int -> Int -> Int) votes.
    largestRemainder quotaFunction votes 0 = map (const 0) votes

"Produce correct number of seats" forall (quotaFunction :: Int -> Int -> Int) votes x.
    sum (largestRemainder quotaFunction votes x) = x
#-}