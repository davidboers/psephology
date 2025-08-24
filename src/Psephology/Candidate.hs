{- | Candidates, like voters, come in both theoretical and real forms. They can be represented on a Euclidean space, or as discrete categories.
There is also an instance of 'Voter' that simply indicates preferences as a list of candidates.
-}
module Psephology.Candidate where

import Data.List (intersperse)

-- | Most of the library's methods require a preset list of @Candidate@ instances.
data Candidate
    = -- | For representing candidates on a Euclidean space.
      Spacial [Double]
    | -- | For representing candidates as discrete categories.
      Categorical String
    | -- | For representing real candidates on the Euclidean space.
      NamedSpacial String [Double]
    deriving (Eq)

instance Show Candidate where
    show (Spacial cs) = unwords $ intersperse "," $ map show cs
    show (Categorical name) = name
    show (NamedSpacial name _) = name

-- | Intended for checking whether the candidate is a theoretical, expressly spacial candidate.
isUnnamed :: Candidate -> Bool
isUnnamed (Spacial _) = True
isUnnamed _ = False
