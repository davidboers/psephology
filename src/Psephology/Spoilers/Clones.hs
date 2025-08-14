module Psephology.Spoilers.Clones where

import Psephology.Candidate
import Psephology.Voter

import Data.List

-- A set of candidates are clones if they are always ranked adjacent to each other. Unless the candidates are identical,
-- there is always a theoretical voter that will intersperse a different candidate, but with few enough voters non-identical
-- candidates can be classified as clones.

clones :: (Voter a) => [Candidate] -> [a] -> [[Int]]
clones candidates voters =
    filter (isClones voters . map (candidates !!)) $
        filter (\s -> length s > 1 && length s < length candidates) $
            subsequences [0 .. length candidates - 1]

isClones :: (Voter a) => [a] -> [Candidate] -> Bool
isClones voters candidates =
    all
        ( \v ->
            let ranks = map (\c -> rank candidates c v) candidates
             in isContinuous ranks
        )
        voters

-- @'isContinuous' l@ returns whether the list @l@ runs in sequence without skipping a number.
isContinuous :: [Int] -> Bool
isContinuous l =
    sort l == [minimum l .. maximum l]
