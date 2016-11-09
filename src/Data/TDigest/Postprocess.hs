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
    ) where

import Prelude ()
import Prelude.Compat
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
histogram :: TDigest comp -> [HistBin]
histogram = iter Nothing 0 . getCentroids
  where
    -- zero
    iter :: Maybe (Mean, Weight) -> Weight -> [(Mean, Weight)] -> [HistBin]
    iter _ _ [] = []
    -- one
    iter Nothing t [(x, w)] = [HistBin x x w t]
    -- first
    iter Nothing t (c1@(x1, w1) : rest@((x2, _) : _))
        = HistBin x1 (mid x1 x2) w1 t : iter (Just c1) (t + w1) rest
    -- middle
    iter (Just (x0, _)) t (c1@(x1, w1) : rest@((x2, _) : _))
        = HistBin (mid x0 x1) (mid x1 x2) w1 t: iter (Just c1) (t + w1) rest
    -- last
    iter (Just (x0, _)) t [(x1, w1)]
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
    iter $ histogram td
  where
    q' = q * totalWeight td

    iter []                          = Nothing
    iter [HistBin a b w t]           = Just $ a + (b - a) * (q' - t) / w
    iter (HistBin a b w t : rest)
        | {- t < q' && -} q' < t + w = Just $ a + (b - a) * (q' - t) / w
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
    iter $ histogram td
  where
    n = totalWeight td

    iter [] = 1
    iter (HistBin a b w t : rest)
        | x < a     = 0
        | x < b     = (t + w * (x - a) / (b - a)) / n
        | otherwise = iter rest
