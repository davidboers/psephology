module Psephology.Spoilers.Utils where

import Psephology.Candidate
import Psephology.ElectoralSystem
import Psephology.Voter

-- Returns the winner if @i@ doesn't participate.
winnerWithout :: (Voter a) => [Candidate] -> [a] -> ElectoralSystem a -> Int -> Int
winnerWithout candidates voters es i =
    let indexes_without = filter (i /=) [0 .. length candidates - 1]
        candidates_without = map (candidates !!) indexes_without
     in indexes_without !! es candidates_without voters