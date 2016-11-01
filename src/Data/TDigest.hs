{-# LANGUAGE ScopedTypeVariables #-}
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

import Data.TDigest.Internal.Type
import Data.TDigest.Postprocess

-------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------

singleton :: KnownNat comp => Double -> TDigest comp
singleton x = insert x emptyTDigest

tdigest :: (Foldable f, KnownNat comp) => f Double -> TDigest comp
tdigest = foldl' (flip insert) emptyTDigest
