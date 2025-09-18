-- | [Wikipedia](https://en.wikipedia.org/wiki/Strategic_voting)
module Psephology.Strategy (Strategy (..)) where

-- | A @Strategy@ is a plan amongst a group of voters to vote insincerely in a way to manipulate
-- the election results. [Gibbard's theorem](https://en.wikipedia.org/wiki/Gibbard%27s_theorem) says
-- that no voting system is immune from manipulation. This does not mean a particular method can be
-- followed in every case, or that all voting systems are equally as susceptible to strategy.
data Strategy a
    = Strategy
    { newWinner :: Int
    -- ^ Winner of the election after the strategy is implemented. Represented by the index of the winning candidate in the input list.
    , participants :: [a]
    -- ^ List of voters that must participate in a strategy.
    }
    deriving (Eq, Show)
