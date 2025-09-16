{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | [Multilevel regression with poststratification](https://en.wikipedia.org/wiki/Multilevel_regression_with_poststratification) (MRP) polling.
--
-- This module supports only binomial distributions. If multiple variables are to be predicted,
-- such as support for multiple political parties, multiple MRPs should be run and the results
-- combined.
module Psephology.Polling.MRP
    ( meanPS
    , out

      -- * Cell
    , Cell (..)
    ) where

import Control.Monad (replicateM, zipWithM)
import Data.List.Extras.Argmax (argmin)
import Data.Random
import GHC.Float (tanDouble)
import System.Random.MWC (createSystemRandom)

-- Cell

data Cell = Cell
    { p :: Double
    -- ^ Between @[0-1]@
    , n :: Int
    -- ^ The **sample size** in this cell (number of respondents).
    , popN :: Int
    -- ^ The **population size** of this cell.
    , specifiers :: [Int]
    }

variance :: Cell -> Double
variance cell =
    1 / (fromIntegral (n cell) * p cell * (1 - p cell))

updateCell :: Model -> Cell -> Cell
updateCell m cell =
    cell {p = logistic $ b0 m + sum (zipWith (!!) (as m) (specifiers cell))}

-- Models

data Model = Model
    { b0 :: Double
    , as :: [[Double]]
    , ss :: [Double]
    }
    deriving (Show)

model :: [Int] -> RVar Model
model limits = do
    b0 <- cauchy 0 1
    ss <- replicateM (length limits) (cauchy 0 0.5)
    as <- zipWithM (\limit s -> replicateM limit $ normal 0 (s ** 2)) limits ss
    return $ Model b0 as ss

weightedLeastSquaresForCell :: Model -> Cell -> Double
weightedLeastSquaresForCell Model {b0, as} cell =
    ((logit (p cell) - b0 - sum (zipWith (!!) as (specifiers cell))) ** 2) / variance cell

weightedLeastSquares :: [Cell] -> [Int] -> Model -> Double
weightedLeastSquares cells limits m =
    sum $ map (weightedLeastSquaresForCell m) cells
        ++ [ (as m !! k !! i) ** 2
           / (ss m !! k) ** 2
           | k <- [0 .. length limits - 1]
           , i <- [0 .. (limits !! k) - 1]
           ]

findLimits :: [Cell] -> [Int]
findLimits cells =
    map (+ 1) $ foldl1 (zipWith max) (map specifiers cells)

fitModel :: Int -> [Cell] -> RVar [Cell]
fitModel iter cells = do
    let limits = findLimits cells
    models <- replicateM iter $ model limits
    let model1 = argmin (weightedLeastSquares cells limits) models
    return $ map (updateCell model1) cells

-- Poststratification

-- | @'meanPS' iter cells@ conducts full regression and stratification. The predicted mean is returned.
meanPS :: Int -> [Cell] -> RVar Double
meanPS iter cells = do
    m <- fitModel iter cells
    let popNs = map (fromIntegral . popN) m
    let phats = map p m
    return $ sum (zipWith (*) popNs phats) / sum popNs

out :: IO ()
out = do
    mwc <- createSystemRandom
    y <- sampleFrom mwc (meanPS 50000 testCells)
    print y

testCells :: [Cell]
testCells =
    [ Cell (12 / 20) 20 1200000 [0, 0]
    , Cell (4 / 10) 10 800000 [0, 1]
    , Cell (6 / 15) 15 800000 [1, 0]
    , Cell (3 / 12) 12 900000 [1, 1]
    , Cell (6 / 8) 8 20000 [2, 0]
    , Cell (2 / 5) 5 300000 [2, 1]
    ]

-- Tests

cauchy :: Double -> Double -> RVar Double
cauchy mu sigma =
    fmap (\r -> mu + sigma * tanDouble (pi * r)) stdUniform

logit :: Double -> Double
logit x = log (x / (1 - x))

logistic :: Double -> Double
logistic alpha =
    1 / (1 + exp (-alpha))
