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
-- Just 990.5
--
-- >>> quantile 0.99 (tdigest [1..1000] :: TDigest 3)
-- Just 990.3...
--
-- t-Digest is more precise in tails, especially median is imprecise:
--
-- >>> median (forceCompress $ tdigest [1..1000] :: TDigest 10)
-- Just 500.5
--
-- === Semigroup
--
-- This operation isn't strictly associative, but statistical
-- variables shouldn't be affected.
--
-- >>> let td xs = tdigest xs :: TDigest 10
--
-- >>> median (td [1..500] <> (td [501..1000] <> td [1001..1500]))
-- Just 750.5
--
-- >>> median ((td [1..500] <> td [501..1000]) <> td [1001..1500])
-- Just 750.5
--
-- The linear is worst-case scenario:
--
-- >>> let td' xs = tdigest (fairshuffle xs) :: TDigest 10
--
-- >>> median (td' [1..500] <> (td' [501..1000] <> td' [1001..1500]))
-- Just 750.5
--
-- >>> median ((td' [1..500] <> td' [501..1000]) <> td' [1001..1500])
-- Just 750.5
--
module Data.TDigest.Vector (
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
    -- >>> let digest = foldl' (flip insert') mempty [0..1000] :: TDigest 5
    -- >>> (size digest, size $ compress digest)
    -- (1001,173)
    --
    -- >>> (quantile 0.1 digest, quantile 0.1 $ compress digest)
    -- (Just 99.6...,Just 99.6...)
    --
    compress,
    forceCompress,

    -- * Statistics
    minimumValue,
    maximumValue,
    -- ** Percentile
    median,
    quantile,
    -- ** Mean & Variance
    --
    -- |
    --
    -- >>> stddev (tdigest $ fairshuffle [0..100] :: TDigest 10)
    -- Just 29.0...
    mean,
    variance,
    stddev,
    -- ** CDF
    cdf,
    icdf,

    -- * Debug
    size,
    valid,
    validate,
    ) where

import Data.TDigest.Vector.Internal
import Data.TDigest.Vector.Postprocess

-- $setup
-- >>> :set -XDataKinds
-- >>> import Prelude.Compat
-- >>> import Data.List.Compat (foldl')
-- >>> import Data.Semigroup ((<>))
--
-- >>> let merge [] ys = []; merge xs [] = xs; merge (x:xs) (y:ys) = x : y : merge xs ys
-- >>> let fairshuffle' xs = uncurry merge (splitAt (Prelude.Compat.length xs `div` 2) xs)
-- >>> let fairshuffle xs = iterate fairshuffle' xs !! 5
