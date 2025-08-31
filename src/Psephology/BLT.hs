-- | See [here](https://yingtongli.me/git/OpenTally/about/docs/blt-fmt.md) for help regarding .blt files.
module Psephology.BLT (fromFile, parse, export) where

import Psephology.Candidate
import Psephology.Utils (split)

import Data.List
import Data.Maybe

-- | @'import' pathName@ returns a tuple of ('[Candidate]', '[[[Candidate]]]'), candidates and voters.
fromFile :: FilePath -> IO ([Candidate], [[[Candidate]]])
fromFile pathName = do
    contents <- readFile pathName
    return $ parse contents

-- Parse

-- | @'parse' text@ parses plain text to election details.
parse :: String -> ([Candidate], [[[Candidate]]])
parse text =
    let -- First line: number of candidates and seats (ignore seats)
        (metaLine, rest) = fromJust $ uncons $ lines text
        no_candidates = read $ head $ words metaLine :: Int

        -- Ballot lines: lines until a line with single '0'
        (ballotLines, afterBallots) = span (/= "0") rest

        -- Parse each ballot line: "num pref1 pref2 ... 0"
        parseBallotLine l =
            let ws = words l
                num = read (head ws) :: Int
                prefs = takeWhile (/= "0") (tail ws)
                prefsIdx = map (map read . split '=') prefs
             in replicate num prefsIdx

        ballotsIdx = concatMap parseBallotLine ballotLines

        -- Candidate names: after the "0" line, next no_candidates lines
        candidateLines = take no_candidates (drop 1 afterBallots)
        candidates = map Categorical candidateLines

        -- Convert ballot indices to Candidate objects
        ballots = map (map (map (\i -> candidates !! (i - 1)))) ballotsIdx
     in (candidates, ballots)

-- | Export to .blt file
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