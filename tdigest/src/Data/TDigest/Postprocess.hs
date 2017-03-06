-- | 'TDigest' postprocessing functions.
--
-- These are re-exported from "Data.TDigest" module.
--
module Data.TDigest.Postprocess (
    -- * Histogram
    histogram,
    HistBin (..),
    -- * Quantiles
    median,
    quantile,
    -- * Mean & variance
    --
    -- | As we have "full" histogram, we can calculate other statistical
    -- variables.
    mean,
    mean',
    variance,
    variance',
    -- * CDF
    cdf,
    icdf,
    -- * NonEmpty
    histogram',
    quantile',
    -- * Debug
    validateHistogram,
    ) where

import Prelude ()
import Prelude.Compat
import Data.Foldable              (toList)
import Data.List.NonEmpty         (NonEmpty (..), nonEmpty)
import Data.Semigroup             (Semigroup (..))
import Data.Semigroup.Foldable    (foldMap1)
import Data.TDigest.Internal.Tree

-------------------------------------------------------------------------------
-- Histogram
-------------------------------------------------------------------------------

-- | Histogram bin
data HistBin = HistBin
    { hbMin       :: !Double  -- ^ lower bound
    , hbMax       :: !Double  -- ^ upper bound
    , hbValue     :: !Double  -- ^ original value: @(mi + ma) / 2@
    , hbWeight    :: !Double  -- ^ weight ("area" of the bar)
    , hbCumWeight :: !Double  -- ^ weight from the right
    }
  deriving (Show)

-- | Calculate histogram based on the 'TDigest'.
histogram :: TDigest comp -> Maybe (NonEmpty HistBin)
histogram = fmap histogram' . nonEmpty . getCentroids

-- | Histogram from centroids
histogram' :: NonEmpty (Mean,Weight) -> NonEmpty HistBin
histogram' = make
  where
    make :: NonEmpty (Mean, Weight) -> NonEmpty HistBin
    -- one
    make ((x, w) :| []) = HistBin x x x w 0 :| []
    -- first
    make (c1@(x1, w1) :| rest@((x2, _) : _))
        = HistBin x1 (mid x1 x2) x1 w1 0 :| iter c1 w1 rest

    -- zero
    iter :: (Mean, Weight) -> Weight -> [(Mean, Weight)] -> [HistBin]
    iter _ _ [] = []
    -- middle
    iter (x0, _) t (c1@(x1, w1) : rest@((x2, _) : _))
        = HistBin (mid x0 x1) (mid x1 x2) x1 w1 t: iter c1 (t + w1) rest
    -- last
    iter (x0, _) t [(x1, w1)]
        = [HistBin (mid x0 x1) x1 x1 w1 t]

    mid a b = (a + b) / 2

-------------------------------------------------------------------------------
-- Quantile
-------------------------------------------------------------------------------

-- | Median, i.e. @'quantile' 0.5@.
median :: TDigest comp -> Maybe Double
median = quantile 0.5

-- | Calculate quantile of a specific value.
quantile :: Double -> TDigest comp -> Maybe Double
quantile q td = quantile' q (totalWeight td) <$> histogram td

-- | Quantile from the histogram.
quantile' :: Double -> Weight -> NonEmpty HistBin -> Double
quantile' q tw = iter . toList
  where
    q' = q * tw

    iter []                          = error "quantile: empty NonEmpty"
    iter [HistBin a b _ w t]           = a + (b - a) * (q' - t) / w
    iter (HistBin a b _ w t : rest)
        | {- t < q' && -} q' < t + w = a + (b - a) * (q' - t) / w
        | otherwise                  = iter rest

-- | Alias of 'quantile'.
icdf :: Double -> TDigest comp -> Maybe Double
icdf = quantile

-------------------------------------------------------------------------------
-- Mean
-------------------------------------------------------------------------------

-- | Mean.
--
-- >>> mean (tdigest [1..100] :: TDigest 10)
-- Just 50.6...
-- 
-- /Note:/ if you only need the mean, calculate it directly.
--
mean :: TDigest comp -> Maybe Double
mean td = mean' <$> histogram td

-- | Mean from the histogram.
mean' :: NonEmpty HistBin -> Double
mean' = getMean . foldMap1 toMean
  where
    toMean (HistBin _ _ x w _) = Mean w x

data Mean' = Mean !Double !Double

getMean :: Mean' -> Double
getMean (Mean _ x) = x

instance Semigroup Mean' where
    Mean w1 x1 <> Mean w2 x2 = Mean w x
      where
        w = w1 + w2
        x = (x1 * w1 + x2 * w2) / w


-- | Variance.
--
variance :: TDigest comp -> Maybe Double
variance td = variance' <$> histogram td

-- | Variance from the histogram.
variance' :: NonEmpty HistBin -> Double
variance' = getVariance . foldMap1 toVariance
  where
    toVariance (HistBin _ _ x w _) = Variance w x 0

data Variance = Variance !Double !Double !Double

getVariance :: Variance -> Double
getVariance (Variance w _ d) = d / (w - 1)

-- See: https://izbicki.me/blog/gausian-distributions-are-monoids
instance Semigroup Variance where
    Variance w1 x1 d1 <> Variance w2 x2 d2 = Variance w x d
      where
        w = w1 + w2
        x = (x1 * w1 + x2 * w2) / w
        d = d1 + d2 + w1 * (x1 * x1) + w2 * (x2 * x2) - w * x * x

-------------------------------------------------------------------------------
-- CDF - cumulative distribution function
-------------------------------------------------------------------------------

-- | Cumulative distribution function.
--
-- /Note:/ if this is the only thing you need, it's more efficient to count
-- this directly.
cdf :: Double -> TDigest comp -> Double
cdf x td =
    iter $ foldMap toList $ histogram td
  where
    n = totalWeight td

    iter [] = 1
    iter (HistBin a b _ w t : rest)
        | x < a     = 0
        | x < b     = (t + w * (x - a) / (b - a)) / n
        | otherwise = iter rest

-------------------------------------------------------------------------------
-- Debug
-------------------------------------------------------------------------------

-- | Validate that list of 'HistBin' is a valid "histogram".
validateHistogram :: Foldable f => f HistBin -> Either String (f HistBin)
validateHistogram bs = traverse validPair (pairs $ toList bs) >> pure bs
  where
    validPair (lb@(HistBin _ lmax _ lwt lcw), rb@(HistBin rmin _ _ _ rcw)) = do
        check (lmax == rmin)     "gap between bins"
        check (lcw + lwt == rcw) "mismatch in weight cumulation"
      where
        check False err = Left $ err ++ " " ++ show (lb, rb)
        check True  _   = Right ()
    pairs xs = zip xs $ tail xs


