-- | [Burying](https://en.wikipedia.org/wiki/Strategic_voting#Burial).
module Psephology.Strategy.Burying where

import Psephology.Candidate
import Psephology.ElectoralSystem (ElectoralSystem)
import Psephology.Strategy
import Psephology.Voter

import Data.List (delete, find, partition, sortOn, subsequences)

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
    let (possibleParticipants, yvoters) = partition (\vi -> preference candidates vi /= y) voters
    let subs = sortOn length $ subsequences [0 .. length possibleParticipants - 1]
    let bury_v = delete (candidates !! y)
    let nonYVoters' sub = map (\i -> if i `elem` sub then bury_v $ possibleParticipants !! i else possibleParticipants !! i) [0 .. length possibleParticipants - 1]
    participantsi <- find (\sub -> y /= es candidates (nonYVoters' sub ++ yvoters)) subs
    let participantsi' = map (possibleParticipants !!) participantsi
    let newWinneri = es candidates (nonYVoters' participantsi ++ yvoters)
    return $ Strategy participantsi' newWinneri