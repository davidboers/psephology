-- | Abstention as a strategy. [Read more](https://en.wikipedia.org/wiki/Participation_criterion).
module Psephology.Strategy.Abstention (abstain, abstainWeak) where

import Data.List (partition)

import Psephology.Candidate
import Psephology.ElectoralSystem (ElectoralSystem)
import Psephology.Strategy
import Psephology.Voter

-- | @'abstain' candidates voters es x@ returns a strategy that ensures @x@ wins an election under @es@.
-- The strategy will involve participants who's first choice is @x@ abstaining from the election.
--
-- If multiple combinations of participants are possible, (one of) the smallest will be returned.
--
-- Will return @Nothing@ if the strategy is impossible. First-past-the-post, the Borda count, and most
-- rated systems are immune from the strategy (assuming no turnout requirement).
abstain :: Voter a => [Candidate] -> [a] -> ElectoralSystem a -> Int -> Maybe (Strategy a)
abstain candidates voters es x =
    let verifyXWins voters' = es candidates voters' == x
     in abstainWorker candidates voters verifyXWins x

-- | Similar to 'abstain', except it doesn't require that @x@ win the election, only that a different
-- candidate wins.
abstainWeak :: Voter a => [Candidate] -> [a] -> ElectoralSystem a -> Int -> Maybe (Strategy a)
abstainWeak candidates voters es x =
    let actualWinner = es candidates voters
        verifyChange voters' = es candidates voters' /= actualWinner
     in abstainWorker candidates voters verifyChange x

abstainWorker :: Voter a => [Candidate] -> [a] -> ([a] -> Bool) -> Int -> Maybe (Strategy a)
abstainWorker candidates voters test x = do
    let (xvoters, othervoters) = partition (\vi -> preference candidates vi == x) voters
    let findAbstainers xvoters'
            | test (xvoters' ++ othervoters) = Just []
            | null xvoters' = Nothing
            | otherwise = do
                let abstainer = head xvoters'
                ls <- findAbstainers $ drop 1 xvoters'
                return (abstainer : ls)
    abstainers <- findAbstainers xvoters
    return $ Strategy abstainers x
