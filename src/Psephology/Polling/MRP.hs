{-# LANGUAGE NamedFieldPuns #-}

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

import Control.Monad (replicateM)
import Data.Random
import GHC.Float (tanDouble)
import System.Random.MWC (createSystemRandom)

import Psephology.Utils (zipWith2D, replace, update)

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
    fromIntegral (n cell) * p cell * (1 - p cell)

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

weightedLeastSquaresForCell :: Model -> Cell -> Double
weightedLeastSquaresForCell Model {b0, as} cell =
    ((logit (p cell) - b0 - sum (zipWith (!!) as (specifiers cell))) ** 2) * variance cell

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

fitModel :: Int -> Double -> [Double] -> [Cell] -> Model
fitModel iter eta ss cells =
    let limits = findLimits cells
        m0     = initModel limits ss
        go 0 m = m
        go t m = go (t-1) (gdStep eta cells m)
     in go iter m0

initModel :: [Int] -> [Double] -> Model
initModel limits ss =
    let as0 = [ replicate m 0.0 | m <- limits ]
     in Model { b0 = 0.0, as = as0, ss = ss }

bumpA :: Int -> Int -> Double -> Model -> Model
bumpA k i delta m@Model{as} =
    m { as = replace as (update (as !! k) (+ delta) i) k }

-- | One gradient-descent step on the penalized WLS objective.
gdStep :: Double -> [Cell] -> Model -> Model
gdStep eta cells m@Model{b0, as} =
    let limits = findLimits cells
        
        loss :: Model -> Double
        loss = weightedLeastSquares cells limits

        db0 = (loss m{ b0 = b0 + eta } - loss m{ b0 = b0 - eta }) / (2 * eta)

        gradAs :: [[Double]]
        gradAs =
            [ [ let mPlus  = bumpA k i  eta m
                    mMinus = bumpA k i (-eta) m
                 in (loss mPlus - loss mMinus) / (2 * eta)
                | i <- [0 .. length (as !! k) - 1]
                ]
            | k <- [0 .. length as - 1]
            ]

        b0' = b0 - eta * db0
        as' = zipWith2D (\aski gradAski -> aski - eta * gradAski) as gradAs
    in m { b0 = b0', as = as' }

-- Poststratification

-- | @'meanPS' iter eta cells@ conducts full regression and stratification. The predicted mean is returned.
meanPS :: Int -> Double -> [Cell] -> RVar Double
meanPS iter eta cells = do
    ss <- map abs <$> replicateM (length cells) (cauchy 0 2.5)
    let m = fitModel iter eta ss cells
        preds = map (updateCell m) cells
        popNs = map (fromIntegral . popN) preds
        phats = map p preds
    return $ sum (zipWith (*) popNs phats) / sum popNs

out :: IO ()
out = do
    mwc <- createSystemRandom
    y <- sampleFrom mwc (meanPS 2000 1e-3 testCells)
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
