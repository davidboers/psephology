module Psephology.NumberOfParties where

import Prelude hiding (pi)

import Psephology.Utils (normalize)

-- | @'effectiveNumberOfParties' p@ returns the effective number of parties. @p@ can be seat counts, 
-- vote counts, or seat/vote shares.
--
-- \[ N = \frac{1}{\sum_{i=1}^{n} p_i^2} \]
effectiveNumberOfParties :: [Double] -> Double
effectiveNumberOfParties p = 
    1 / sum [ pi ** 2 | pi <- normalize $ filter (> 0) p ] 