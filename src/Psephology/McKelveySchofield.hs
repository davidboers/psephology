-- | This sub-module is dedicated to the [McKelvey-Schofield chaos theorem](https://en.wikipedia.org/wiki/McKelvey%E2%80%93Schofield_chaos_theorem).
module Psephology.McKelveySchofield (findASpoiler, newMajority) where

import Psephology.Candidate (Candidate (Spacial))
import Psephology.Condorcet
import Psephology.Voter (preference)

import Data.List
import Data.Maybe (fromJust)

{- | @'findASpoiler' p1 voters@ returns a new policy that is preferred by a majority of @voters@ to the current policy @p1@.
This solution does not produce the optimal spoiler, but only the first one found.
-}
findASpoiler :: Candidate -> [[Double]] -> Candidate
findASpoiler p1 voters =
    let
        dimensions = minimum $ map length voters

        makeBounds d =
            let values = map (!! d) voters
             in [minimum values, minimum values + 0.5 .. maximum values]

        candidates = map Spacial $ transpose $ map makeBounds [0 .. dimensions - 1]

        preferred p2 =
            pairwiseMaj voters p2 p1 > 0
     in
        fromJust $ find preferred candidates

-- | @'newMajority' p1 voters p2@ returns a list of the indexes of @voters@ that prefer @p2@ over @p1@.
newMajority :: Candidate -> [[Double]] -> Candidate -> [Int]
newMajority p1 voters p2 =
    filter (\i -> preference [p2, p1] (voters !! i) == 0) [0 .. length voters - 1]