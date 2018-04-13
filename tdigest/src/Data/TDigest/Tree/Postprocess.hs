-- | 'TDigest' postprocessing functions.
--
-- These are re-exported from "Data.TDigest" module.
--
module Data.TDigest.Tree.Postprocess (
    -- * Quantiles
    median,
    quantile,
    -- * Mean & variance
    --
    -- | As we have "full" histogram, we can calculate other statistical
    -- variables.
    mean,
    variance,
    stddev,
    -- * CDF
    cdf,
    icdf,
    ) where

import Prelude ()
import Prelude.Compat

import Data.TDigest.Tree.Internal

import qualified Data.TDigest.Postprocess as PP

-------------------------------------------------------------------------------
-- Quantile
-------------------------------------------------------------------------------

-- | Median, i.e. @'quantile' 0.5@.
median :: TDigest comp -> Maybe Double
median = PP.median

-- | Calculate quantile of a specific value.
quantile :: Double -> TDigest comp -> Maybe Double
quantile = PP.quantile

-------------------------------------------------------------------------------
-- Mean
-------------------------------------------------------------------------------

-- | Mean.
--
-- >>> mean (tdigest [1..100] :: TDigest 10)
-- Just 50.5
--
-- /Note:/ if you only need the mean, calculate it directly.
--
mean :: TDigest comp -> Maybe Double
mean = PP.mean

-- | Variance.
--
variance :: TDigest comp -> Maybe Double
variance = PP.variance

-- | Standard deviation, square root of variance.
stddev :: TDigest comp -> Maybe Double
stddev = PP.stddev

-------------------------------------------------------------------------------
-- CDF - cumulative distribution function
-------------------------------------------------------------------------------

-- | Cumulative distribution function.
--
-- /Note:/ if this is the only thing you need, it's more efficient to count
-- this directly.
cdf :: Double -> TDigest comp -> Double
cdf = PP.cdf

-- | An alias for 'quantile'
icdf :: Double -> TDigest comp -> Maybe Double
icdf = quantile
