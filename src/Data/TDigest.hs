{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- A new data structure for accurate on-line accumulation of rank-based
-- statistics such as quantiles and trimmed means.
--                .
-- See original paper: "Computing extremely accurate quantiles using t-digest"
-- by Ted Dunning and Otmar Ertl for more details
-- <https://github.com/tdunning/t-digest/blob/master/docs/t-digest-paper/histo.pdf>.
--
-- === Examples
--
-- >>> median (tdigest [1..100] :: TDigest 10)
-- Just 50.5
--
module Data.TDigest (
    -- * Construction
    TDigest,
    tdigest,

    -- ** Population
    singleton,
    insert,

    -- * Statistics
    -- ** Histogram
    histogram,
    HistBin (..),
    -- ** Percentile
    median,
    percentile,
    ) where

import Prelude ()
import Prelude.Compat
import Data.List      (foldl')
import GHC.TypeLits   (KnownNat)

import Data.TDigest.Internal.Ref
import Data.TDigest.Postprocess

-------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------

-- | Make a 'TDigest' of a single data point.
singleton :: KnownNat comp => Double -> TDigest comp
singleton x = insert x emptyTDigest

-- | Strict 'foldl'' over 'Foldable' structure.
tdigest :: (Foldable f, KnownNat comp) => f Double -> TDigest comp
tdigest = foldl' (flip insert) emptyTDigest

-- | Insert single value into 'TDigest'.
insert
    :: KnownNat comp
    => Double         -- ^ element
    -> TDigest comp
    -> TDigest comp
insert x = compress . insertCentroid (x, 1)
