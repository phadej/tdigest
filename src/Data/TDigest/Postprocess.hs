-- | 'TDigest' postprocessing functions.
--
-- These are re-exported from "Data.TDigest" module.
--
module Data.TDigest.Postprocess (
    -- * Histogram
    histogram,
    HistBin (..),
    -- * Percentiles
    median,
    quantile,
    ) where

import Prelude ()
import Prelude.Compat

import qualified Data.Map.Strict as Map

import Data.TDigest.Internal.Ref

-------------------------------------------------------------------------------
-- Histogram
-------------------------------------------------------------------------------

data HistBin = HistBin
    { hbMin    :: !Double  -- ^ lower bound
    , hbMax    :: !Double  -- ^ max bound
    , hbWeight :: !Double  -- ^ weight ("area" of the bar)
    }
  deriving (Show)

-- | Calculate histogram based on the 'TDigest'.
histogram :: TDigest comp -> [HistBin]
histogram (TDigest centroids _) = iter Nothing $ Map.toList centroids
  where
    -- zero
    iter _ [] = []
    -- one
    iter Nothing [(x, w)] = [HistBin x x w]
    -- first
    iter Nothing (c1@(x1, w1) : rest@((x2, _) : _))
        = HistBin x1 (mid x1 x2) w1 : iter (Just c1) rest
    -- middle
    iter (Just (x0, _)) (c1@(x1, w1) : rest@((x2, _) : _))
        = HistBin (mid x0 x1) (mid x1 x2) w1 : iter (Just c1) rest
    -- last
    iter (Just (x0, _)) [(x1, w1)]
        = [HistBin (mid x0 x1) x1 w1]

    mid a b = (a + b) / 2

-------------------------------------------------------------------------------
-- Percentile
-------------------------------------------------------------------------------

-- | Median, i.e. @'quantile' 0.5@.
median :: TDigest comp -> Maybe Double
median = quantile 0.5

-- | Calculate quantile of a specific value.
quantile
    :: Double
    -> TDigest comp
    -> Maybe Double
quantile q td@(TDigest _centroids n) =
    iter 0 $ histogram td
  where
    q' = q * n

    iter _ []                        = Nothing
    iter _ [HistBin a _b _]          = Just a
    iter t (HistBin a b w : rest)
        | {- t < q' && -} q' < t + w = Just $ a + (b - a) * (q' - t) / w
        | otherwise                  = iter (t + w) rest
