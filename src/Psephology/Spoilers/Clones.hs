{- | Clones are candidates that are ranked adjacent to one another by every voter. They exist primarily in theory, where
they are used to describe the [independence of clones criterion](https://en.wikipedia.org/wiki/Independence_of_clones_criterion).

Clones are a type of spoiler, and the independence of clones criterion a weak version of independence of irrelevant alternatives (IIA).
Because they are so rare in practice, the proxy is a more helpful concept when analyzing political candidates with similar ideological
inclinations.

In the single-peaked preferences model, unless candidates \(A\) and \(B\) are literally identical (\(\|A-B\|=0\)), there is always a
theoretical voter that will intersperse a different candidate \(C\). Whether a pair of candidates are clones depends only on the
behavior of the voters, so it is possible for similar but not identical candidates to be clones.
-}
module Psephology.Spoilers.Clones (clones, isClones) where

import Psephology.Candidate
import Psephology.Voter

import Data.List

{- | Returns lists of indexes of clone candidates. The function evaluates all possible subsequences of the input list, so some of the
output lists may be subsequences of each other.
-}
clones :: Voter a => [Candidate] -> [a] -> [[Int]]
clones candidates voters =
    filter (isClones voters . map (candidates !!)) $
        filter (\s -> length s > 1 && length s < length candidates) $
            subsequences [0 .. length candidates - 1]

-- | Returns @True@ if all candidates in the input list are clones of each other.
isClones :: Voter a => [a] -> [Candidate] -> Bool
isClones voters candidates =
    all
        ( \v ->
            let ranks = map (rank candidates v) candidates
             in isContinuous ranks
        )
        voters

-- @'isContinuous' l@ returns whether the list @l@ runs in sequence without skipping a number.
isContinuous :: [Int] -> Bool
isContinuous l =
    sort l == [minimum l .. maximum l]
