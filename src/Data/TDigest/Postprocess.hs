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
import Data.TDigest.Internal.Tree

-------------------------------------------------------------------------------
-- Histogram
-------------------------------------------------------------------------------

-- | Histogram bin
data HistBin = HistBin
    { hbMin       :: !Double  -- ^ lower bound
    , hbMax       :: !Double  -- ^ upper bound
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
    make ((x, w) :| []) = HistBin x x w 0 :| []
    -- first
    make (c1@(x1, w1) :| rest@((x2, _) : _))
        = HistBin x1 (mid x1 x2) w1 0 :| iter c1 w1 rest

    -- zero
    iter :: (Mean, Weight) -> Weight -> [(Mean, Weight)] -> [HistBin]
    iter _ _ [] = []
    -- middle
    iter (x0, _) t (c1@(x1, w1) : rest@((x2, _) : _))
        = HistBin (mid x0 x1) (mid x1 x2) w1 t: iter c1 (t + w1) rest
    -- last
    iter (x0, _) t [(x1, w1)]
        = [HistBin (mid x0 x1) x1 w1 t]

    mid a b = (a + b) / 2

-------------------------------------------------------------------------------
-- Quantile
-------------------------------------------------------------------------------

-- | Median, i.e. @'quantile' 0.5@.
median :: TDigest comp -> Maybe Double
median = quantile 0.5

-- | Calculate quantile of a specific value.
quantile :: Double -> TDigest comp -> Maybe Double
quantile q td =
    fmap (quantile' q (totalWeight td)) $ histogram td

-- | Quantile from histogram.
quantile' :: Double -> Weight -> NonEmpty HistBin -> Double
quantile' q tw = iter . toList
  where
    q' = q * tw

    iter []                          = error "quantile: empty NonEmpty"
    iter [HistBin a b w t]           = a + (b - a) * (q' - t) / w
    iter (HistBin a b w t : rest)
        | {- t < q' && -} q' < t + w = a + (b - a) * (q' - t) / w
        | otherwise                  = iter rest

-- | Alias of 'quantile'.
icdf :: Double -> TDigest comp -> Maybe Double
icdf = quantile

-------------------------------------------------------------------------------
-- CDF - cumulative distribution function
-------------------------------------------------------------------------------

-- | Cumulative distribution function.
--
-- /Note:/ if this is the only thing you need, it's more efficient to count
-- this directly.
cdf :: Double -> TDigest comp -> Double
cdf x td =
    iter $ undefined $ histogram td
  where
    n = totalWeight td

    iter [] = 1
    iter (HistBin a b w t : rest)
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
    validPair (lb@(HistBin _ lmax lwt lcw), rb@(HistBin rmin _ _ rcw)) = do
        check (lmax == rmin)     "gap between bins"
        check (lcw + lwt == rcw) "mismatch in weight cumulation"
      where
        check False err = Left $ err ++ " " ++ show (lb, rb)
        check True  _   = Right ()
    pairs xs = zip xs $ tail xs


