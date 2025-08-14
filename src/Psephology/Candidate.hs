module Psephology.Candidate where

import Data.List (intersperse)

data Candidate
    = Spacial [Double]
    | Categorical String
    | NamedSpacial String [Double]
    deriving (Eq)

instance Show Candidate where
    show (Spacial cs) = unwords $ intersperse "," $ map show cs
    show (Categorical name) = name
    show (NamedSpacial name _) = name

isUnnamed :: Candidate -> Bool
isUnnamed (Spacial _) = True
isUnnamed _ = False
