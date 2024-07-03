{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
-- | 'TDigest' postprocessing functions.
--
-- These are re-exported from "Data.TDigest" module.
--
module Data.TDigest.Postprocess.Internal (
    -- * Histogram
    HasHistogram (..),
    HistBin (..),
    histogramFromCentroids,
    -- * Quantiles
    quantile,
    -- * Mean & variance
    --
    -- | As we have "full" histogram, we can calculate other statistical
    -- variables.
    mean,
    variance,
    -- * CDF
    cdf,
    -- * Debug
    validateHistogram,
    -- * Affine - internal
    Affine (..),
    ) where

import Data.Foldable         (toList, traverse_)
import Data.Foldable1        (foldMap1)
import Data.Functor.Compose  (Compose (..))
import Data.Functor.Identity (Identity (..))
import Data.List.NonEmpty    (NonEmpty (..), nonEmpty)
import Data.Proxy            (Proxy (..))
import Data.Semigroup        (Semigroup (..))

import qualified Data.List.NonEmpty as NE

import Data.TDigest.Internal

-------------------------------------------------------------------------------
-- Histogram
-------------------------------------------------------------------------------

-- | Histogram bin
data HistBin = HistBin
    { hbMin       :: !Mean    -- ^ lower bound
    , hbMax       :: !Mean    -- ^ upper bound
    , hbValue     :: !Mean    -- ^ original value: @(mi + ma) / 2@
    , hbWeight    :: !Weight  -- ^ weight ("area" of the bar)
    , hbCumWeight :: !Weight  -- ^ weight from the right, excludes this bin
    }
  deriving (Show)

-- | Types from which we can extract histogram.
class Affine f => HasHistogram a f | a -> f where
    histogram   :: a -> f (NonEmpty HistBin)
    totalWeight :: a -> Weight

instance (HistBin ~ e) => HasHistogram (NonEmpty HistBin) Identity where
    histogram = Identity
    totalWeight = tw . NE.last where
        tw hb =  hbWeight hb + hbCumWeight hb

instance (HistBin ~ e) => HasHistogram [HistBin] Maybe where
    histogram = nonEmpty
    totalWeight = affine 0 totalWeight . histogram

-- | Histogram from centroids
histogramFromCentroids :: NonEmpty Centroid -> NonEmpty HistBin
histogramFromCentroids = make
  where
    make :: NonEmpty Centroid -> NonEmpty HistBin
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

-- | Quantile from the histogram.
quantile :: Double -> Weight -> NonEmpty HistBin -> Double
quantile q tw = iter . toList
  where
    q' = q * tw

    iter []                          = error "quantile: empty NonEmpty"
    iter [HistBin a b _ w t]           = a + (b - a) * (q' - t) / w
    iter (HistBin a b _ w t : rest)
        | {- t < q' && -} q' < t + w = a + (b - a) * (q' - t) / w
        | otherwise                  = iter rest

-------------------------------------------------------------------------------
-- Mean
-------------------------------------------------------------------------------

-- | Mean from the histogram.
mean :: NonEmpty HistBin -> Double
mean = getMean . foldMap1 toMean
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

-- | Variance from the histogram.
variance :: NonEmpty HistBin -> Double
variance = getVariance . foldMap1 toVariance
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
cdf :: Double
    -> Double  -- ^ total weight
    -> [HistBin] -> Double
cdf x n = iter
  where
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
validateHistogram bs = traverse_ validPair (pairs $ toList bs) >> pure bs
  where
    validPair (lb@(HistBin _ lmax _ lwt lcw), rb@(HistBin rmin _ _ _ rcw)) = do
        check (lmax == rmin)     "gap between bins"
        check (lcw + lwt == rcw) "mismatch in weight cumulation"
      where
        check False err = Left $ err ++ " " ++ show (lb, rb)
        check True  _   = Right ()
    pairs xs = zip xs $ tail xs

-------------------------------------------------------------------------------
-- Affine
-------------------------------------------------------------------------------

-- | Affine containers, i.e. containing at most 1 element
--
-- This class doesn't have 'traverse' analogie
-- as it would require using 'Pointed' which is disputed type class.
--
-- > traverseAff :: Pointed f => (a -> f b) -> t a -> f (t b)
--
class Traversable t => Affine t where
    -- | Like `foldMap`
    affine :: b -> (a -> b) -> t a -> b
    affine x f = fromAffine x . fmap f

    fromAffine :: a -> t a -> a
    fromAffine x = affine x id

    {-# MINIMAL fromAffine | affine #-}

instance Affine Identity    where fromAffine _ = runIdentity
instance Affine Maybe       where affine = maybe
instance Affine Proxy       where affine x _ _ = x

-- | Composition of 'Affine' containers is 'Affine'
instance (Affine f, Affine g) => Affine (Compose f g) where
    affine x f (Compose c) = affine x (affine x f) c
