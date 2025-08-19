module Psephology.Spoilers.Proxies where

import Data.List (subsequences)

import Psephology.Candidate
import Psephology.ElectoralSystem
import Psephology.Spoilers.Utils
import Psephology.Voter

-- Returns a list of proxy pairs.
proxies :: (Voter a) => [Candidate] -> [a] -> ElectoralSystem a -> [(Int, Int)]
proxies candidates voters es =
    [ (a, b)
    | [a, b] <- filter (\sub -> length sub == 2) $ subsequences [0 .. length candidates - 1]
    , isProxy candidates voters es a b
    ]

-- @a@ is a proxy for @b@ if the exclusion of @a@ results in the election of @b@ and the exclusion of @b@ results in the election of @a@.
isProxy :: (Voter a) => [Candidate] -> [a] -> ElectoralSystem a -> Int -> Int -> Bool
isProxy candidates voters es a b =
    let without_a = winnerWithout candidates voters es a
        without_b = winnerWithout candidates voters es b
     in without_a == b && without_b == a
