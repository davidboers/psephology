module Psephology.Pathologies where

import Psephology.Candidate
import Psephology.Condorcet
import Psephology.ElectoralSystem
import Psephology.Utils
import Psephology.Voter

import Data.List

isPathology :: (Voter a) => [Candidate] -> [a] -> ElectoralSystem a -> Maybe Int -> Bool
isPathology candidates voters es i =
    let winner = es candidates voters
     in maybe False (winner /=) i

-- | @'condorcetFailure' candidates voters es@ returns True if there is a Condorcet failure (system @es@ elects someone who is not the Condorcet winner, assuming there is one).
condorcetFailure :: (Voter a) => [Candidate] -> [a] -> ElectoralSystem a -> Bool
condorcetFailure candidates voters es =
    isPathology candidates voters es (condorcetWinner candidates voters)

findMajority :: (Voter a) => [Candidate] -> [a] -> [Int] -> Maybe Int
findMajority candidates voters tally =
    find (\i -> tally !! i >= majority voters) [0 .. length candidates - 1]

-- | Returns the majority winner (winner of a majority of first preferences) if there is one.
majorityWinner :: (Voter a) => [Candidate] -> [a] -> Maybe Int
majorityWinner candidates voters =
    let tally = votes candidates voters
     in findMajority candidates voters tally

-- | @'majorityFailure' candidates voters es@ returns True if the 'majorityWinner' (assuming there is one) does not win the election under @es@.
majorityFailure :: (Voter a) => [Candidate] -> [a] -> ElectoralSystem a -> Bool
majorityFailure candidates voters es =
    isPathology candidates voters es (majorityWinner candidates voters)

-- | Returns the majority loser (candidate disliked the most by a majority of voters) if there is one.
majorityLoser :: (Voter a) => [Candidate] -> [a] -> Maybe Int
majorityLoser candidates voters =
    let tally = antiVotes candidates voters
     in findMajority candidates voters tally

-- | @'solidCoalition' candidates voters l@ returns a (possibly empty) subset of @voters@ that prefers all candidates in @l@ to all others.
solidCoalition :: (Voter a) => [Candidate] -> [a] -> [Int] -> [a]
solidCoalition candidates voters l =
    filter (isPrefersSet candidates l) voters

-- | @'isPrefersSet' candidates l v@ returns True if voter @v@ prefers all candidates in @l@ to all other candidates.
isPrefersSet :: (Voter a) => [Candidate] -> [Int] -> a -> Bool
isPrefersSet candidates l v =
    let anti_l = map (candidates !!) $ filter (`notElem` l) [0 .. length candidates - 1]
        least_preferred_l = lastPreference (map (candidates !!) l) v
        preferred_anti_l = preference anti_l v
     in preference [candidates !! least_preferred_l, candidates !! preferred_anti_l] v == least_preferred_l

-- | Returns a list of 'solidCoalition's that are supported by a majority of @voters@.
majorityCoalitions :: (Voter a) => [Candidate] -> [a] -> [[Int]]
majorityCoalitions candidates voters =
    let subs = filter (\sub -> length sub > 1) $ subsequences [0 .. length candidates - 1]
        coalitions = map (solidCoalition candidates voters) subs
     in map (subs !!) $ filter (\i -> length (coalitions !! i) >= majority voters) [0 .. length subs - 1]

-- | Returns True if the winning candidate falls outside at least one majority coalition.
mutualMajorityFailure :: (Voter a) => [Candidate] -> [a] -> ElectoralSystem a -> Bool
mutualMajorityFailure candidates voters es =
    let coalitions = majorityCoalitions candidates voters
        winner = es candidates voters
     in not $ all (winner `elem`) coalitions
