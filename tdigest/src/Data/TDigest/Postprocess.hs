module Data.TDigest.Postprocess (
    -- * Histogram
    I.HasHistogram (..),
    I.HistBin (..),
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
    -- * Affine
    I.Affine (..)
    ) where

import Prelude ()
import Prelude.Compat
import qualified Data.List.NonEmpty  as NE

import qualified Data.TDigest.Postprocess.Internal as I

-- | Median, i.e. @'quantile' 0.5@.
median :: I.HasHistogram a f => a -> f Double
median = quantile 0.5

-- | Calculate quantile of a specific value.
quantile :: I.HasHistogram a f => Double -> a -> f Double
quantile q x = I.quantile q (I.totalWeight x) <$> I.histogram x

-- | Mean.
--
-- >>> mean (Tree.tdigest [1..100] :: Tree.TDigest 10)
-- Just 50.5
--
-- /Note:/ if you only need the mean, calculate it directly.
--
mean :: I.HasHistogram a f => a -> f Double
mean x = I.mean <$> I.histogram x

-- | Variance.
--
variance :: I.HasHistogram a f => a -> f Double
variance x = I.variance <$> I.histogram x

-- | Standard deviation, square root of variance.
stddev :: I.HasHistogram a f => a -> f Double
stddev = fmap sqrt . variance

-- | Cumulative distribution function.
--
-- /Note:/ if this is the only thing you need, it's more efficient to count
-- this directly.
cdf :: I.HasHistogram a f => Double -> a -> Double
cdf q x = I.affine 1 (I.cdf q (I.totalWeight x) . NE.toList) $ I.histogram x

-- | An alias for 'quantile'.
icdf :: I.HasHistogram a f => Double -> a -> f Double
icdf = quantile

-- $setup
-- >>> :set -XDataKinds
-- >>> import qualified Data.TDigest.Tree as Tree
