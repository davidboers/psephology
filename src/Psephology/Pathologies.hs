module Psephology.Pathologies
    ( condorcetFailure
    , majorityFailure
    , mutualMajorityFailure
    , smithFailure

      -- * Helpers
    , majorityWinner
    , majorityLoser
    , majorityCoalitions
    , solidCoalition
    , isPrefersSet
    ) where

import Psephology.Candidate
import Psephology.Condorcet
import Psephology.Counting
import Psephology.Quotas
import Psephology.Voter

import Data.List
import Data.Function ((&))

-- Pathologies

-- | @'condorcetFailure' winner candidates voters@ returns @True@ if @winner@ is not a Condorcet winner. If there is no Condorcet winner, returns @False@.
condorcetFailure :: Voter a => Int -> [Candidate] -> [a] -> Bool
condorcetFailure winner candidates voters =
    maybe False (winner /=) $ condorcetWinner candidates voters
    
-- | @'majorityFailure' winner candidates voters@ returns @True@ if @winner@ is not the 'majorityWinner'. If there is no majority winner, returns @False@.
majorityFailure :: Voter a => Int -> [Candidate] -> [a] -> Bool
majorityFailure winner candidates voters =
    maybe False (winner /=) $ majorityWinner candidates voters
    
-- | Returns True if the winning candidate falls outside any majority coalition. Excludes majority coalitions that are proper subsets of other majority coalitions.
mutualMajorityFailure
    :: Voter a
    => Int
    -- ^ Index of winning candidate.
    -> [Candidate]
    -> [a]
    -> Bool
mutualMajorityFailure winner candidates voters =
    let coalitions = majorityCoalitions candidates voters
        nonSubsets = filter (\coalition -> not $ any (isProperSubset coalition) coalitions) coalitions
     in not $ all (winner `elem`) nonSubsets

-- | Returns True if the winning candidate is not a member of the [Smith set](https://en.wikipedia.org/wiki/Smith_set).
smithFailure
    :: Voter a
    => Int
    -- ^ Index of winning candidate.
    -> [Candidate]
    -> [a]
    -> Bool
smithFailure winner candidates voters =
    winner `notElem` smithSet candidates voters

-- Helpers

findMajority :: Voter a => [a] -> [Int] -> Maybe Int
findMajority voters =
    findIndex (>= majority voters)

-- | Returns the majority winner (winner of a majority of first preferences) if there is one.
majorityWinner :: Voter a => [Candidate] -> [a] -> Maybe Int
majorityWinner candidates voters =
    findMajority voters $ votes candidates voters

-- | Returns the majority loser (candidate disliked the most by a majority of voters) if there is one.
majorityLoser :: Voter a => [Candidate] -> [a] -> Maybe Int
majorityLoser candidates voters =
    findMajority voters $ antiVotes candidates voters

-- | Returns a list of 'solidCoalition's that are supported by a majority of @voters@.
majorityCoalitions :: Voter a => [Candidate] -> [a] -> [[Int]]
majorityCoalitions candidates voters =
    filter (\sub -> length sub > 1 && length sub < length candidates) $ subsequences [0 .. length candidates - 1]
        & filter (\sub -> length (solidCoalition candidates voters sub) >= majority voters)

-- | @'solidCoalition' candidates voters l@ returns a (possibly empty) subset of @voters@ that prefers all candidates in @l@ to all others.
solidCoalition :: Voter a => [Candidate] -> [a] -> [Int] -> [a]
solidCoalition candidates voters l =
    filter (isPrefersSet candidates l) voters

-- | @'isPrefersSet' candidates l v@ returns True if voter @v@ prefers all candidates in @l@ to all other candidates.
isPrefersSet :: Voter a => [Candidate] -> [Int] -> a -> Bool
isPrefersSet _ [] _ = True
isPrefersSet candidates l v =
    let anti_l = [0 .. length candidates - 1] \\ l
        least_preferred_l = l !! lastPreference (map (candidates !!) l) v
        preferred_anti_l
            | pref == -1 = -1
            | otherwise = anti_l !! pref
            where
                pref = preference (map (candidates !!) anti_l) v
     in ( (preferred_anti_l == -1)
            || (preference [candidates !! least_preferred_l, candidates !! preferred_anti_l] v == 0)
        )

isProperSubset :: Eq a => [a] -> [a] -> Bool
isProperSubset rhs lhs =
    isSubsequenceOf rhs lhs && rhs /= lhs