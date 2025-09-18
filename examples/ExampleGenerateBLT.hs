module Main where

import Control.Monad (zipWithM_)

import Psephology.BLT (export)
import Psephology.Candidate
import Psephology.Counting (votes)
import Psephology.ElectoralSystems.Runoff (instantRunoffVoting)
import Psephology.ElectoralSystems.Condorcet (rankedPairs)
import Psephology.ProportionalRepresentation.HighestAverages (highestAveragesWithInit, dhondt)
import Psephology.SinglePeakedPreferences (singlePeakedVotersNormalCentered, singlePeakedVotersNormalLim, formalize)

import Data.List (zip4, partition)

{-# ANN main "HLint: use head" #-}
main :: IO ()
main = do
    candidates <- singlePeakedVotersNormalCentered 1000 2
    let (as1, others1) = partition (\x -> x !! 0 > 50 && x !! 1 > 50) candidates
    let (bs1, others2) = partition (\x -> x !! 0 > 50) $ others1 ++ drop 140 as1
    let (cs1, others3) = partition (\x -> x !! 1 > 50) $ others2 ++ drop 140 bs1
    let ds1            = others3 ++ drop 140 cs1
    let as = gather 2 $ map Spacial $ take 140 as1
    let bs = gather 2 $ map Spacial $ take 140 bs1
    let cs = gather 2 $ map Spacial $ take 140 cs1
    let ds = gather 2 $ map Spacial $ take 140 ds1
    let others4 = drop 140 ds1
    let is = take 85 others4
    avs <- singlePeakedVotersNormalLim 100 [70, 60] 210000 2
    bvs <- singlePeakedVotersNormalLim 100 [40, 40] 140000 2
    let candidates' = map (\(a, b, c, d) -> concat [a, b, c, d]) $ zip4 as bs cs ds
    let votes' = interweave avs bvs
    let votesByDistrict = gather 5000 votes'
    let formalizedVotersByDistrict = zipWith formalize candidates' votesByDistrict
    let names = map show [1..70]
    let fileContents = zipWith3 export candidates' formalizedVotersByDistrict names
    --zipWithM_ writeFile (map (\fn -> "examples/" ++ fn ++ ".blt") names) fileContents
    writeFile "examples/heatmaps/voters.csv" $ unlines $ map (concatMap (\x -> show x ++ ",")) votes'
    let winners = zipWith instantRunoffVoting candidates' formalizedVotersByDistrict
    let firstSeats = [ length (filter (\i -> 0 <= i && i <= 1) winners)
                     , length (filter (\i -> 2 <= i && i <= 3) winners)
                     , length (filter (\i -> 4 <= i && i <= 5) winners)
                     , length (filter (\i -> 6 <= i && i <= 7) winners)
                     ]

    putStrLn "Constituency Seats"
    putStrLn $ "A: " ++ show (firstSeats !! 0)
    putStrLn $ "B: " ++ show (firstSeats !! 1)
    putStrLn $ "C: " ++ show (firstSeats !! 2)
    putStrLn $ "D: " ++ show (firstSeats !! 3)

    let partyVotes = votes (zipWith NamedSpacial ["A", "B", "C", "D"] [[51, 51], [51, 49], [49, 51], [49, 49]]) votes'
    let totalSeats = highestAveragesWithInit firstSeats dhondt partyVotes 70
    putStrLn "Party votes"
    print partyVotes
    putStrLn "Total seats"
    print totalSeats

interweave :: [a] -> [a] -> [a]
interweave (x:xs) (y:ys) = x : y : interweave xs ys
interweave []     ys     = ys
interweave xs     []     = xs

gather :: Int -> [a] -> [[a]]
gather _ [] = []
gather n l =
    take n l : gather n (drop n l)
