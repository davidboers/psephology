{-# LANGUAGE NamedFieldPuns #-}

-- | [Multilevel regression with poststratification](https://en.wikipedia.org/wiki/Multilevel_regression_with_poststratification) (MRP) polling.
--
-- This module supports only binomial distributions. If multiple variables are to be predicted,
-- such as support for multiple political parties, multiple MRPs should be run and the results
-- combined.
--
-- ==== Specifications
--
-- Below is a comprehensive description of the implementation in mathematical form. For the 
-- multilevel regression phase, the below is repeated for a given number of iterations:
--
-- \[
--      w_c = n_c\tilde{y}_c(1-\tilde{y}_c)\\
--      r_c = \text{logit}(\tilde{y}_c)-\beta_0-\sum_{k}{\alpha_{k,\text{spec}_c[k]}}\\
--      \frac{\partial\mathcal{L}}{\partial\beta_0} = -2\sum_c{w_cr_c}\\
--      \frac{\partial\mathcal{L}}{\partial\alpha_{ki}} = -2\sum_c\{w_cr_c\text{if spec}_c[k] = i\}+2\frac{\alpha_{ki}}{\sigma_k^2}\\
--      \beta_0 := \beta_0-\eta\frac{\partial\mathcal{L}}{\partial\beta_0}\\
--      \alpha_{ki} := \alpha_{ki}-\eta\frac{\partial\mathcal{L}}{\partial\alpha_{ki}}\\
-- \]
--
-- With \(\beta_0,\alpha_{ki}\) preset to 0 and \(\sigma\) determined by a half-Cauchy 
-- distribution.
--
-- The random coefficients \(\beta_0\) and \(\alpha\) are then used to re-estimate \(y\).
--
-- \[ \hat{y}_c = \text{logit}^{-1}(\beta_0+\sum_{k}{a_{k,\text{spec}_c[k]}}) \]
--
-- Finally, the poststratification:
--
-- \[ \hat{\Theta} = \frac{\sum_cN_c\hat{y}_c}{\sum_cN_c} \]
--
-- Given:
--
--      * A list of cells where:
--              * \(n_c\) ('n') is the number of respondents in that cell.
--              * \(y_c\) ('y') is the proportion (percentage of \(n\)) of respondents in that cell 
--              that responded in the affirmative. This is the posterior prior mean.
--              * \(N_c\) ('popN') is the number of people in the cell, for the purpose of stratification.
--              Usually obtained from census data, etc.  
--              * \(\text{spec}_c\) ('specifiers') is a list containing the representations for that cell 
--              for each rule \(k\).
--      * An \(\eta\) in range \((0,1)\). Try starting with \(1e^{-3}\). The \(\alpha\) values 
--      should not leave the range \([-1,1]\), and if they do, try reducing \(\eta\) by 10 times. 
--      * A number of iterations usually derived from \(\eta\). Should not exceed \(\frac{2}{\eta}\), and
--      often far less iterations are required. If runtime becomes an issue, you can limit the 
--      iteration count to the point where the regression coefficients "settle", or don't change 
--      much with each additional iteration.
--
-- \(y\) should be adjusted by Laplace smoothing:
--
-- \[ \tilde{y} = \frac{y+0.5}{n+1} \]
--
-- The above step is necessary to prevent divide-by-zero errors.
--
-- In plain English, the process involves a Gradient descent to estimate the random coefficients 
-- \(\beta_0\) and \(\alpha\). Once the coefficients are estimated, we can use the new linear 
-- regression to estimate cell means, and then stratify. The implementation is entirely 
-- Haskell-native and does not use foreign libraries. \(\text{Cauchy}\), \(\text{logit}\), and 
-- \(\text{logit}^{-1}\) also have local implementations defined by:
--
-- \[
--      \text{Cauchy}(\mu, \gamma) \sim \mu+\gamma\times\tan(\pi r), r \sim \text{Uniform}(0, 1)\\
--      \sigma_k \sim \text{abs}\circ\text{Cauchy}(0, 2.5)\\
--      \text{logit } p = \ln\frac{p}{1-p}\\
--      \text{logit}^{-1} \alpha = \frac{1}{1+\exp(-\alpha)}\\
-- \]
--
-- ==== Calling
-- 
-- @
--  import Data.Random
--  Import System.Random.MWC (createSystemRandom)
--
--  cells :: ['Cell']
--  cells = -- y    n  N    spec
--      [ 'Cell' 0.32 50 1000 [0, 0]
--      , 'Cell' 0.45 20 5000 [0, 1]
--      , 'Cell' 0.10 10 7000 [1, 0]
--      , 'Cell' 0.25 25 3000 [1, 1]
--      , 'Cell' 0.60 75  500 [2, 0]
--      , 'Cell' 0.65 90  200 [2, 1]
--      ]
--
--  main = do
--      let eta = 1e-3
--          iter = 2 / eta
--      mwc <- createSystemRandom
--      thetaHat <- sampleFrom mwc ('meanPS' iter eta cells)
--      print thetaHat
-- @
--
-- If @1@ is returned, @eta@ is too high. Try lowering to @1e-4@, then @1e-5@, etc.
--
module Psephology.Polling.MRP
    ( meanPS

      -- * Cell
    , Cell (..)
    ) where

import Control.Monad (replicateM)
import Data.Random (stdUniform, RVar)
import GHC.Float (tanDouble)

import Psephology.Utils (zipWith2D)

-- Cell

data Cell = Cell
    { y :: Double
    -- ^ Between @[0-1]@
    , n :: Int
    -- ^ The **sample size** in this cell (number of respondents).
    , popN :: Int
    -- ^ The **population size** of this cell.
    , specifiers :: [Int]
    }

variance :: Cell -> Double
variance cell =
    fromIntegral (n cell) * y cell * (1 - y cell)

residual :: Model -> Cell -> Double
residual Model {b0, as} cell = 
    logit (y cell) - b0 - sum (zipWith (!!) as (specifiers cell))

updateCell :: Model -> Cell -> Cell
updateCell Model {b0, as} cell =
    cell {y = logistic $ b0 + sum (zipWith (!!) as (specifiers cell))}

-- Models

data Model = Model
    { b0 :: Double
    , as :: [[Double]]
    , ss :: [Double]
    }
    deriving (Show)

findLimits :: [Cell] -> [Int]
findLimits cells =
    foldl1 (zipWith max) (map specifiers cells)

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

-- | One gradient-descent step on the penalized WLS objective.
gdStep :: Double -> [Cell] -> Model -> Model
gdStep eta cells m@Model{b0, as, ss} =
    let limits = findLimits cells
        ws = map variance cells
        rs = map (residual m) cells

        gb0 = (-2) * sum (zipWith (*) ws rs)
        gas =
            [ 
                [ (-2) * sum [ w * r | (c, w, r) <- zip3 cells ws rs, specifiers c !! k == i] 
                +   2  * (as !! k !! i) / (s * s)
                | i <- [0..(limits !! k)]
                ]
            | (k, s) <- zip [0..length limits - 1] ss
            ]

    in m 
        { b0 = b0 - eta * gb0
        , as = zipWith2D (\aki gaki -> aki - eta * gaki) as gas
        }

-- Poststratification

-- | @'meanPS' iter eta cells@ conducts full regression and stratification. The predicted mean is returned.
-- 
-- See module docs.
meanPS :: Int -> Double -> [Cell] -> RVar Double
meanPS iter eta cells = do
    ss <- map abs <$> replicateM (length cells) (cauchy 0 2.5)
    let m = fitModel iter eta ss cells
        preds = map (updateCell m) cells
        popNs = map (fromIntegral . popN) preds
        yhats = map y preds
    return $ sum (zipWith (*) popNs yhats) / sum popNs

-- Helpers

cauchy :: Double -> Double -> RVar Double
cauchy mu gamma =
    fmap (\r -> mu + gamma * tanDouble (pi * r)) stdUniform

logit :: Double -> Double
logit x = log (x / (1 - x))

logistic :: Double -> Double
logistic alpha =
    1 / (1 + exp (-alpha))
