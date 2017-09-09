{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
-- | This is non empty version of 'Data.TDigest.TDigest', i.e. this is not a 'Monoid',
-- but on the other hand, 'quantile' returns 'Double'  not @'Maybe' 'Double'@.
--
-- See "Data.TDigest" for documentation. The exports should be similar,
-- sans non-'Maybe' results.
--
-- === Examples
--
-- >>> quantile 0.99 (tdigest (1 :| [2..1000]) :: TDigest 25)
-- 990.5
--
-- >>> quantile 0.99 (tdigest (1 :| [2..1000]) :: TDigest 3)
-- 989.0...
--
-- t-Digest is more precise in tails, especially median is imprecise:
--
-- >>> median (forceCompress $ tdigest (1 :| [2..1000]) :: TDigest 25)
-- 497.6...
--
module Data.TDigest.Tree.NonEmpty (
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
    totalWeight,
    minimumValue,
    maximumValue,
    -- ** Percentile
    median,
    quantile,
    -- ** Mean & variance
    mean,
    variance,
    stddev,
    -- ** CDF
    cdf,
    icdf,
    ) where

import Prelude ()
import Prelude.Compat

import Control.DeepSeq         (NFData (..))
import Control.Monad           (when)
import Data.Binary             (Binary (..))
import Data.Functor.Identity   (Identity (..))
import Data.Semigroup          (Semigroup (..))
import Data.Semigroup.Foldable (Foldable1)
import Data.Semigroup.Reducer  (Reducer (..))
import GHC.TypeLits            (KnownNat)

import           Data.TDigest.Internal
import qualified Data.TDigest.Postprocess   as PP
import qualified Data.TDigest.Tree.Internal as T

newtype TDigest comp = TDigest { unEmpty :: T.TDigest comp }

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance NFData (TDigest comp) where
    rnf (TDigest t) = rnf t

instance Show (TDigest comp) where
    showsPrec d (TDigest t) = showsPrec d t

instance KnownNat comp => Semigroup (TDigest comp) where
    TDigest a <> TDigest b = TDigest (a <>  b)

instance KnownNat comp => Reducer Double (TDigest comp) where
    cons = insert
    snoc = flip insert
    unit = singleton

instance KnownNat comp => Binary (TDigest comp) where
    get = do
        t <- get
        when (T.size t <= 0) $ fail "empty TDigest.NonEmpty"
        return (TDigest t)

    put (TDigest t) = put t

instance PP.HasHistogram (TDigest comp) Identity where
    histogram   = maybe (error "NonEmpty.histogram") Identity . PP.histogram . unEmpty
    totalWeight = PP.totalWeight . unEmpty

-------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------

overTDigest :: (T.TDigest c -> T.TDigest c) -> TDigest c -> TDigest c
overTDigest f = TDigest . f . unEmpty

singleton :: KnownNat comp => Double -> TDigest comp
singleton = TDigest . T.singleton

insert :: KnownNat comp => Double -> TDigest comp -> TDigest comp
insert x = TDigest . T.insert x . unEmpty

insert' :: KnownNat comp => Double -> TDigest comp -> TDigest comp
insert' x =  overTDigest $ T.insert' x

compress :: forall comp. KnownNat comp => TDigest comp -> TDigest comp
compress = overTDigest T.compress

forceCompress :: forall comp. KnownNat comp => TDigest comp -> TDigest comp
forceCompress = overTDigest T.forceCompress

minimumValue :: TDigest comp -> Mean
minimumValue = T.minimumValue . unEmpty

maximumValue :: TDigest comp -> Mean
maximumValue = T.maximumValue . unEmpty

totalWeight :: TDigest comp -> Weight
totalWeight = T.totalWeight . unEmpty

median :: TDigest comp -> Double
median = runIdentity . PP.median

quantile :: Double -> TDigest comp -> Double
quantile q = runIdentity . PP.quantile q

mean :: TDigest comp -> Double
mean = runIdentity . PP.mean

variance :: TDigest comp -> Double
variance = runIdentity . PP.variance

stddev :: TDigest comp -> Double
stddev = runIdentity . PP.variance

-- | Alias of 'quantile'.
icdf :: Double -> TDigest comp -> Double
icdf = quantile

cdf :: Double -> TDigest comp -> Double
cdf = PP.cdf

tdigest :: (Foldable1 f, KnownNat comp) => f Double -> TDigest comp
tdigest = TDigest . T.tdigest

-- $setup
-- >>> :set -XDataKinds
-- >>> import Prelude.Compat
-- >>> import Data.List.NonEmpty (NonEmpty (..))
-- >>> import Data.List.Compat (foldl')
--
-- >>> let merge [] ys = []; merge xs [] = xs; merge (x:xs) (y:ys) = x : y : merge xs ys
-- >>> let fairshuffle' xs = uncurry merge (splitAt (length xs `div` 2) xs)
-- >>> let fairshuffle xs = iterate fairshuffle' xs !! 5
