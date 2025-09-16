{-# LANGUAGE FlexibleInstances #-}

-- | See [here](https://yingtongli.me/git/OpenTally/about/docs/blt-fmt.md) for help regarding .blt files.
module Psephology.BLT
    ( -- * General
      ReadableVoter(..)

      -- * Importing
    , fromFile
    , parse

      -- * Exporting
    , export
    ) where

import Psephology.Candidate
import Psephology.Utils (split)

import Data.List
import Data.Maybe

-- | Only real voters can be read from a .blt file. There are two instances of this class, intended
-- to facilitate both weak- and total-ordered preferences.
--
-- @
--      'fromFile' "foo.blt" :: IO (['Candidate'], [['Candidate']])   -- For total ordering (no tied ranks)
--      'fromFile' "bar.blt" :: IO (['Candidate'], [[['Candidate']]]) -- For weak ordering (w/tied ranks)
-- @
--
-- If the blt file parsed for total ordering contains tied rankings, the candidates will be preferenced
-- in whatever order they appear in the file. Ex. "A=B" will be read as "A, then B".
class ReadableVoter a where
    parsePrefs :: [Candidate] -> [String] -> a

    writePrefs :: [Candidate] -> a -> [String]

instance ReadableVoter [Candidate] where
    parsePrefs candidates strs = concat (parsePrefs candidates strs :: [[Candidate]])

    writePrefs candidates = map (show . ci candidates)

instance ReadableVoter [[Candidate]] where
    parsePrefs candidates = map (map (\s -> candidates !! (read s - 1)) . split '=')

    writePrefs candidates = map (intercalate "=" . map (show . ci candidates))

ci :: [Candidate] -> Candidate -> Int
ci candidates c =
    fromJust (elemIndex c candidates) + 1

-- Importing

-- | @'fromFile' pathName@ returns a tuple of candidates and voters.
fromFile :: ReadableVoter a => FilePath -> IO ([Candidate], [a])
fromFile pathName =
    parse <$> readFile pathName

-- | @'parse' text@ parses plain text to election details.
parse :: ReadableVoter a => String -> ([Candidate], [a])
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
                prefsPart = takeWhile (/= "0") (tail ws)
             in replicate num $ parsePrefs candidates prefsPart

        ballots = concatMap parseBallotLine ballotLines

        -- Candidate names: after the "0" line, next no_candidates lines
        candidateLines = take no_candidates (drop 1 afterBallots)
        candidates = map Categorical candidateLines
     in (candidates, ballots)

-- Exporting

-- | Export to .blt file
export :: (ReadableVoter a, Eq a) => [Candidate] -> [a] -> String -> String
export candidates voters title =
    let ballotLine prefs =
            let num = length $ filter (prefs ==) voters
             in unwords $ show num : writePrefs candidates prefs ++ ["0"]

        header = show (length candidates) ++ " 1"
        ballots = map ballotLine $ nub voters
        names
            | any isUnnamed candidates = map (\i -> "Candidate " ++ show i) [1 .. length candidates]
            | otherwise = map show candidates
        footer = "0" : map show names ++ [show title]
     in unlines $ header : ballots ++ footer