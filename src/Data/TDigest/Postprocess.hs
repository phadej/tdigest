module Data.TDigest.Postprocess (
    -- * Histogram
    histogram,
    HistBin (..),
    -- * Percentiles
    median,
    percentile,
    ) where

import Prelude ()
import Prelude.Compat

import qualified Data.Map as Map

import Data.TDigest.Internal.Type

-------------------------------------------------------------------------------
-- Histogram
-------------------------------------------------------------------------------

data HistBin = HistBin
    { hbMin    :: !Double
    , hbMax    :: !Double
    , hbWeight :: !Double
    }
  deriving (Show)

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

-- | Median, i.e. @'percentile' 0.5@.
median :: TDigest comp -> Maybe Double
median = percentile 0.5

-- | Calculate percentile of a specific value.
percentile
    :: Double
    -> TDigest comp
    -> Maybe Double
percentile q td@(TDigest _centroids n) =
    iter 0 $ histogram td
  where
    q' = q * n

    iter _ []                        = Nothing
    iter _ [HistBin a _b _]          = Just a
    iter t (HistBin a b w : rest)
        | {- t < q' && -} q' < t + w = Just $ a + (b - a) * (q' - t) / w
        | otherwise                  = iter (t + w) rest
