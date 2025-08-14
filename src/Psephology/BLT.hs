module Psephology.BLT where

import Psephology.Candidate

import Data.List
import Data.Maybe

export :: [Candidate] -> [[Candidate]] -> String -> String
export candidates voters title =
    let prefOrdering :: [Candidate] -> String
        prefOrdering o =
            let num = length $ filter (o ==) voters
             in unwords $ map show $ num : map ci o ++ [0]

        ci :: Candidate -> Int
        ci c =
            fromJust (elemIndex c candidates) + 1

        header = show (length candidates) ++ " 1"
        ballots = map prefOrdering $ nub voters
        names
            | any isUnnamed candidates = map (\i -> "Candidate " ++ show i) [1 .. length candidates]
            | otherwise = map show candidates
        footer = "0" : map show names ++ [show title]
     in unlines $ header : ballots ++ footer