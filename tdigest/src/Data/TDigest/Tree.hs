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
-- Just 989.0...
--
-- t-Digest is more precise in tails, especially median is imprecise:
--
-- >>> median (forceCompress $ tdigest [1..1000] :: TDigest 25)
-- Just 497.6...
--
-- === Semigroup
--
-- This operation isn't strictly associative, but statistical
-- variables shouldn't be affected.
--
-- >>> let td xs = tdigest xs :: TDigest 10
--
-- >>> median (td [1..500] <> (td [501..1000] <> td [1001..1500]))
-- Just 802...
--
-- >>> median ((td [1..500] <> td [501..1000]) <> td [1001..1500])
-- Just 726...
--
-- The linear is worst-case scenario:
--
-- >>> let td' xs = tdigest (fairshuffle xs) :: TDigest 10
--
-- >>> median (td' [1..500] <> (td' [501..1000] <> td' [1001..1500]))
-- Just 750.3789...
--
-- >>> median ((td' [1..500] <> td' [501..1000]) <> td' [1001..1500])
-- Just 750.3789...
--
module Data.TDigest.Tree (
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
    -- (1001,52)
    --
    -- >>> (quantile 0.1 digest, quantile 0.1 $ compress digest)
    -- (Just 99.6...,Just 89.7...)
    --
    -- /Note:/ when values are inserted in more random order,
    -- t-Digest self-compresses on the fly:
    --
    -- >>> let digest = foldl' (flip insert') mempty (fairshuffle [0..1000]) :: TDigest 10
    -- >>> (size digest, size $ compress digest, size $ forceCompress digest)
    -- (78,78,48)
    --
    -- >>> quantile 0.1 digest
    -- Just 98.9...
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
    -- -- >>> stddev (tdigest $ fairshuffle [0..100] :: TDigest 10)
    -- Just 29.1...
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
    debugPrint,
    ) where

import Data.TDigest.Tree.Internal
import Data.TDigest.Tree.Postprocess

-- $setup
-- >>> :set -XDataKinds
-- >>> import Data.Foldable (foldl')
-- >>> import Data.Semigroup ((<>))
--
-- >>> let merge [] ys = []; merge xs [] = xs; merge (x:xs) (y:ys) = x : y : merge xs ys
-- >>> let fairshuffle' xs = uncurry merge (splitAt (Prelude.length xs `div` 2) xs)
-- >>> let fairshuffle xs = iterate fairshuffle' xs !! 5
