module Psephology.Spoilers (module Psephology.Spoilers.Clones, module Psephology.Spoilers.Proxies, spoilers, isSpoiler, spoilee) where

import Data.Maybe

import Psephology.Candidate
import Psephology.ElectoralSystem
import Psephology.Voter

import Psephology.Spoilers.Clones
import Psephology.Spoilers.Proxies
import Psephology.Spoilers.Utils

spoilers :: (Voter a) => [Candidate] -> [a] -> ElectoralSystem a -> [Int]
spoilers candidates voters es =
    filter (isSpoiler candidates voters es) [0 .. length candidates - 1]

-- Does a spoilee exist when spoiler @i@ doesn't participate? @i@ cannot be a spoiler if they win with all candidates competing.
isSpoiler :: (Voter a) => [Candidate] -> [a] -> ElectoralSystem a -> Int -> Bool
isSpoiler candidates voters es i
    | es candidates voters == i = False
    | otherwise = isJust $ spoilee candidates voters es i

-- Returns the candidate for whom the election is spoiled if @i@ doesn't participate, or Nothing if @i@ isn't a spoiler.
spoilee :: (Voter a) => [Candidate] -> [a] -> ElectoralSystem a -> Int -> Maybe Int
spoilee candidates voters es i =
    let winner_with = es candidates voters
        winner_without = winnerWithout candidates voters es i
     in if winner_with /= winner_without
            then Just winner_without
            else Nothing
