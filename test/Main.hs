{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Data.Bifunctor qualified
import Data.List (elemIndex, find, intersperse, nub, sort, sortBy, sortOn, subsequences)
import Data.List.NonEmpty (sortWith)
import Data.Maybe
import Data.Ord (Down (Down), comparing)
import GHC.OldList (findIndex)

import Psephology.BLT
import Psephology.Candidate
import Psephology.Condorcet
import Psephology.ElectoralSystem
import Psephology.Pathologies
import Psephology.SampleElections
import Psephology.SinglePeakedPreferences
import Psephology.Spoilers
import Psephology.Spoilers.Clones
import Psephology.Spoilers.Proxies
import Psephology.Voter

eachSystem :: (Voter a, Show b) => ([Candidate] -> [a] -> ElectoralSystem a -> b) -> [Candidate] -> [a] -> String
eachSystem f candidates voters =
    unlines $
        map
            (\(name, es) -> "   " ++ name ++ ": " ++ show (f candidates voters es))
            [ ("FPTP", firstPastThePost)
            , ("Anti-plurality", antiPlurality)
            , ("TRS", twoRound)
            , ("IRV", instantRunoffVoting)
            , ("Borda", bordaCount)
            , ("Dowdall", dowdallSystem)
            ]

proxyPairLists :: (Voter a) => [Candidate] -> [a] -> ElectoralSystem a -> [(Candidate, Candidate)]
proxyPairLists candidates voters es =
    map (Data.Bifunctor.bimap (candidates !!) (candidates !!)) $ proxies candidates voters es

main :: IO ()
main = do
    let voters = tennesseeCapital
    let candidates = tennesseeCapitalCandidates
    -- FPTP: Memphis
    -- Anti-plurality: Nashville
    -- TRS: Nashville
    -- IRV: Knoxville
    -- Borda: Nashville
    -- Dowdall: Nashville
    putStrLn "Winners: "
    putStrLn $ eachSystem (\_ _ es -> candidates !! es candidates voters) candidates voters

    putStr "Condorcet winner: " -- Nashville
    print $ candidates !! fromJust (condorcetWinner candidates voters)

    putStr "Pairwise majorities: "
    print $ condorcetMatrix numPreferOver candidates voters

    putStrLn ".blt file: "
    putStr $ export candidates voters "Unnamed"

    putStrLn "Clones: " -- [[Chattanooga,Knoxville],[Nashville,Chattanooga,Knoxville]]
    print $ map (map (candidates !!)) $ clones candidates voters

    putStrLn "Spoilers: "
    putStrLn $ eachSystem (\_ _ es -> map (candidates !!) $ spoilers candidates voters es) candidates voters

    putStrLn "Proxies: "
    putStrLn $ eachSystem proxyPairLists candidates voters
