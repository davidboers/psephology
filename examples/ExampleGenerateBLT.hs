module Main where

import Control.Monad (zipWithM_)

import Psephology.BLT (export)
import Psephology.Candidate
import Psephology.SinglePeakedPreferences (singlePeakedVotersNormalCentered, singlePeakedVotersNormalLim, formalize)

import Data.List (zip4, partition)

main :: IO ()
main = do
    candidates <- singlePeakedVotersNormalCentered 1000 2
    let (as1, others1) = partition (\x -> head x > 50 && x !! 1 > 50) candidates
    let (bs1, others2) = partition (\x -> head x > 50) $ others1 ++ drop 140 as1
    let (cs1, others3) = partition (\x -> x !! 1 > 50) $ others2 ++ drop 140 bs1
    let ds1            = others3 ++ drop 140 cs1
    let as = gather 2 $ map Spacial $ take 140 as1
    let bs = gather 2 $ map Spacial $ take 140 bs1
    let cs = gather 2 $ map Spacial $ take 140 cs1
    let ds = gather 2 $ map Spacial $ take 140 ds1
    let others4 = drop 140 ds1
    let is = take 85 others4
    avs <- singlePeakedVotersNormalLim 100 [60, 55] 1925000 2
    bvs <- singlePeakedVotersNormalLim 100 [37.5, 37.5] 1575000 2
    let candidates' = map (\(a, b, c, d) -> concat [a, b, c, d ]) $ zip4 as bs cs ds
    let votes = interweave avs bvs
    let votesByDistrict = gather 50000 votes 
    let formalizedVotersByDistrict = zipWith formalize candidates' votesByDistrict
    let names = map show [1..70]
    let fileContents = zipWith3 export candidates' formalizedVotersByDistrict names
    zipWithM_ writeFile (map (\fn -> "examples/" ++ fn ++ ".blt") names) fileContents

interweave :: [a] -> [a] -> [a]
interweave (x:xs) (y:ys) = x : y : interweave xs ys
interweave []     ys     = ys
interweave xs     []     = xs

gather :: Int -> [a] -> [[a]]
gather _ [] = []
gather n l =
    take n l : gather n (drop n l)
