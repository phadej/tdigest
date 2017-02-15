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
-- >>> median (tdigest [1..100] :: TDigest 3)
-- Just 49.098408814428936
--
module Data.TDigest (
    -- * Construction
    TDigest,
    tdigest,

    -- ** Population
    singleton,
    insert,
    insert',

    -- * Compression
    compress,
    forceCompress,

    -- * Statistics
    -- ** Histogram
    histogram,
    HistBin (..),
    -- ** Percentile
    median,
    quantile,
    -- ** CDF
    cdf,
    icdf,

    -- * Debug
    valid,
    validate,
    ) where

import Prelude ()
import Prelude.Compat
import Data.Foldable    (toList)
import Data.List.Compat (foldl')
import GHC.TypeLits     (KnownNat)

import Data.TDigest.Internal.Tree
import Data.TDigest.Postprocess

-------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------

-- | Make a 'TDigest' of a single data point.
singleton :: KnownNat comp => Double -> TDigest comp
singleton x = insert x emptyTDigest

-- | Strict 'foldl'' over 'Foldable' structure.
tdigest :: (Foldable f, KnownNat comp) => f Double -> TDigest comp
tdigest = forceCompress . foldl' insertChunk emptyTDigest . chunks . toList
  where
    -- compress after each chunk, forceCompress at the very end.
    insertChunk td xs =
        compress (foldl' (flip insert') td xs)

    chunks [] = []
    chunks xs =
        let (a, b) = splitAt 1000 xs -- 1000 is totally arbitrary.
        in a : chunks b

-- | Insert single value into 'TDigest'.
insert
    :: KnownNat comp
    => Double         -- ^ element
    -> TDigest comp
    -> TDigest comp
insert x = compress . insert' x

-- | Insert single value, don't compress 'TDigest' even if needed.
--
-- For sensibly bounded input, it makes sense to let 'TDigest' grow (it might
-- grow linearly in size), and after that compress it once.
insert'
    :: KnownNat comp
    => Double         -- ^ element
    -> TDigest comp
    -> TDigest comp
insert' x = insertCentroid (x, 1)
