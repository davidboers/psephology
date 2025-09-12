module Main where

import Data.List (intercalate)

import Psephology.Parliament
import Psephology.ElectoralSystems.Borda
import Psephology.ElectoralSystems.Condorcet
import Psephology.ElectoralSystems.Plurality
import Psephology.ElectoralSystems.Rated
import Psephology.ElectoralSystems.Runoff
import Psephology.ElectoralSystems.Sortition

import System.IO (writeFile)

main :: IO ()
main = do
    p1 <- generate 100 [70, 60] 5 60 2 1000
    p2 <- generate 100 [40, 40] 5 40 2 1000
    let parliament = merge p1 p2
    writeFile "examples/heatmaps/voters.csv" $ votersCSV parliament
    writeFile "examples/heatmaps/elected.csv" $ electedsCSV parliament
    -- 10 minutes
    --print $ pathologies parliament
    putStrLn "Sample parliament generated and written to files."

csvify :: Show a => [[a]] -> String
csvify = unlines . map (intercalate "," . map show)

csvHeader :: [String] -> String
csvHeader header = intercalate "," header ++ "\n"

votersCSV :: Parliament [Double] -> String
votersCSV parliament =
    csvify $ concatMap (\(Election _ voters) -> voters) parliament

electedsCSV :: Parliament [Double] -> String
electedsCSV parliament =
    let header = ["fptp_x", "fptp_y", "trs_x", "trs_y", "irv_x", "irv_y", "borda_x", "borda_y", "copeland_x", "copeland_y", "apv_x", "apv_y"]
        underDifferentMethods (Election candidates voters) = map (candidates !!)
            [ firstPastThePost    candidates voters
            , twoRound            candidates voters
            , instantRunoffVoting candidates voters
            , bordaCount          candidates voters
            , copelandLlull       candidates voters
            , approvalVoting      candidates voters
            ] 
     in csvHeader header ++ csvify (map underDifferentMethods parliament)