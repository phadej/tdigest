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
-- >>> quantile 0.99 (tdigest [1..1000] :: TDigest 25)
-- Just 990.499...
--
-- >>> quantile 0.99 (tdigest [1..1000] :: TDigest 3)
-- Just 992.7...
--
-- t-Digest is more precise in tails, especially median is imprecise:
--
-- >>> median (tdigest [1..1000] :: TDigest 25)
-- Just 502.5...
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
    --
    -- |
    --
    -- >>> let digest = foldl' (flip insert') mempty [0..1000] :: TDigest 10
    -- >>> (size digest, size $ compress digest)
    -- (1001,54)
    --
    -- >>> (quantile 0.1 digest, quantile 0.1 $ compress digest)
    -- (Just 99.6...,Just 90.1...)
    --
    -- /Note:/ when values are inserted in more random order,
    -- t-Digest self-compresses on the fly:
    --
    -- >>> let digest = foldl' (flip insert') mempty (fairshuffle [0..1000]) :: TDigest 10
    -- >>> (size digest, size $ compress digest, size $ forceCompress digest)
    -- (77,77,44)
    --
    -- >>> quantile 0.1 digest
    -- Just 96.9...
    --
    compress,
    forceCompress,

    -- * Statistics
    totalWeight,
    minimumValue,
    maximumValue,
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
    validateHistogram,
    ) where

import Prelude ()
import Prelude.Compat ()

import Data.TDigest.Internal.Tree
import Data.TDigest.Postprocess

-- $setup
-- >>> :set -XDataKinds
-- >>> import Prelude.Compat
-- >>> import Data.List.Compat (foldl')
--
-- >>> let merge [] ys = []; merge xs [] = xs; merge (x:xs) (y:ys) = x : y : merge xs ys
-- >>> let fairshuffle' xs = uncurry merge (splitAt (length xs `div` 2) xs)
-- >>> let fairshuffle xs = iterate fairshuffle' xs !! 5
