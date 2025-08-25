-- | [Burying](https://en.wikipedia.org/wiki/Strategic_voting#Burial).
module Psephology.Strategy.Burying where

import Psephology.Candidate
import Psephology.ElectoralSystem (ElectoralSystem)
import Psephology.Strategy
import Psephology.Voter

import Data.List (delete, find, subsequences)

-- | @'bury' candidates voters es y@ returns a strategy that ensures @y@ loses an election under @es@.
-- The strategy will involve each participant ranking @y@ at the bottom of their ballot.
--
-- The function is as time efficient as possible, but becomes increasingly complex as the number of voters
-- increases. The more participants needed to make the strategy work, the longer it will take to devise. If
-- multiple combinations of participants are possible, (one of) the smallest will be returned.
--
-- Will return @Nothing@ if the strategy is impossible. First-past-the-post, the two-round system, and instant
-- runoff voting are immune from burying. [Read more](https://en.wikipedia.org/wiki/Later-no-harm_criterion).
bury :: [Candidate] -> [[Candidate]] -> ElectoralSystem [Candidate] -> Int -> Maybe (Strategy [Candidate])
bury candidates voters es y = do
    let votersForOtherCandidates = filter (\vi -> preference candidates vi /= y) voters
    let bury_v = delete (candidates !! y)
    -- This is stupid I don't know why I thought this would work
    participantsi <- find (\vs -> y /= es candidates (map bury_v vs)) (subsequences votersForOtherCandidates)
    let newWinneri = es candidates participantsi
    return $ Strategy participantsi newWinneri