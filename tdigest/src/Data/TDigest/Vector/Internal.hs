{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Data.TDigest.Vector.Internal where

import Control.DeepSeq        (NFData (..))
import Data.Either            (isRight)
import Data.Foldable          (toList)
import Data.List              (foldl', sortBy)
import Data.List.NonEmpty     (nonEmpty)
import Data.Ord               (comparing)
import Data.Proxy             (Proxy (..))
import Data.Semigroup         (Semigroup (..))
import Data.Semigroup.Reducer (Reducer (..))
import GHC.TypeLits           (KnownNat, Nat, natVal)

import qualified Data.Vector.Unboxed as VU

import           Data.TDigest.Internal
import qualified Data.TDigest.Postprocess.Internal as PP

-- import Debug.Trace
--
-- | 'TDigest' is a vector of centroids plus not yet merged elements.
--
-- The size of structure is dictated by @compression@, *𝛿*. And is *O(𝛿)*.
--
data TDigest (compression :: Nat) = TDigest
    { tdigestTotalWeight :: !Size                  -- ^ sum of vector and buffer size
    , tdigestData        :: !(VU.Vector Centroid)  -- ^ actual data. *Invariants:* sorted by mean; length <= 2 𝛿 (soft)
    , tdigestBufferSize  :: !Size
    , tdigestBuffer      :: [Double]               -- ^ addition buffer, elements with weight 1. *Invariants:* length 2 <= 𝛿
    , tdigestDirection   :: !Bool                  -- ^ direction is a hack, so we merge from left and right. *TODO* remove?
    }
  deriving Show

instance KnownNat comp => Semigroup (TDigest comp) where
    (<>) = combineTDigest

instance KnownNat comp => Monoid (TDigest comp) where
    mempty = emptyTDigest
    mappend = (<>)

-- | Both 'cons' and 'snoc' are 'insert'
instance KnownNat comp => Reducer Double (TDigest comp) where
    cons = insert
    snoc = flip insert
    unit = singleton

instance NFData (TDigest comp) where
    rnf (TDigest _ _ _ b _) = rnf b

instance KnownNat comp => PP.HasHistogram (TDigest comp) Maybe where
    histogram = fmap PP.histogramFromCentroids . nonEmpty . VU.toList . tdigestData . finalize
    totalWeight = totalWeight

-- | Size of structure
size :: TDigest comp -> Int
size td = VU.length (tdigestData td) + tdigestBufferSize td

totalWeight :: TDigest comp -> Weight
totalWeight = fromIntegral . tdigestTotalWeight

-- | Center of left-most centroid. Note: may be different than min element inserted.
--
-- >>> minimumValue (tdigest [1..100] :: TDigest 3)
-- 1.0
--
minimumValue :: KnownNat comp => TDigest comp -> Mean
minimumValue td
    | VU.null d = posInf
    | otherwise = fst (VU.head d)
  where
    d = tdigestData (finalize td)

-- | Center of right-most centroid. Note: may be different than max element inserted.
--
-- >>> maximumValue (tdigest [1..100] :: TDigest 3)
-- 100.0
--
maximumValue :: KnownNat comp => TDigest comp -> Mean
maximumValue td
    | VU.null d = posInf
    | otherwise = fst (VU.last d)
  where
    d = tdigestData (finalize td)

-------------------------------------------------------------------------------
-- Mapping function
-------------------------------------------------------------------------------

-- | Mapping from quantile *q* to notional index *k* with compression parameter *𝛿*.
--
-- >>> ksize 42 0
-- 0.0
--
-- >>> ksize 42 1
-- 42.0
--
-- *q@ is clamped.:
--
-- >>> ksize 42 2
-- 42.0
--
ksize
    :: Double  -- ^ compression parameter, 𝛿
    -> Double  -- ^ quantile, q
    -> Double  -- ^ notional index, k
ksize comp q = comp * (asin (2 * clamp q - 1) / pi  + 0.5)

clamp :: Double -> Double
clamp x
    | x < 0.0   = 0.0
    | x > 1.0   = 1.0
    | otherwise = x

-- | Inverse of 'ksize'.
--
-- >>> ksizeInv 42 0
-- 0.0
--
-- >>> ksizeInv 42 42
-- 1.0
--
-- >>> ksizeInv 42 (ksize 42 0.3)
-- 0.3
--
ksizeInv
    :: Double  -- ^ compression parameter, 𝛿
    -> Double  -- ^ notional index, k
    -> Double  -- ^ quantile, q
ksizeInv comp k
    | k > comp = 1
    | k < 0    = 0
    | otherwise = 0.5 * (sin ((k / comp - 0.5) * pi) + 1)

-------------------------------------------------------------------------------
-- Merging
-------------------------------------------------------------------------------

merge :: Int -> Double -> [(Mean, Weight)] -> [(Mean, Weight)]
merge _   _    []     = []
merge tw' comp (y:ys) = go 0 (qLimit' 0) y ys
  where
    -- total weight
    tw = fromIntegral tw'

    qLimit' :: Double -> Double
    qLimit' q0 = ksizeInv comp (ksize comp q0 + 1)  -- k⁻¹ (k (q₀, 𝛿) + 1, 𝛿)

    go :: Double         -- q0
       -> Double         -- qLimit
       -> (Mean, Weight)   -- sigma
       -> [(Mean, Weight)]
       -> [(Mean, Weight)]
    go _q0 _qLimit sigma [] = [sigma] -- C'.append(σ)
    go  q0  qLimit sigma (x:xs)
        | q <= qLimit = go q0 qLimit (plus sigma x) xs
        | otherwise   = sigma : go q0' (qLimit' q0') x xs
-- traceShow ("q", sigma, x, q, qLimit) $
      where
        q = q0 + (snd sigma + snd x) / tw
        q0' = q0 + snd sigma / tw

    plus :: Centroid -> Centroid -> Centroid
    plus (m1,w1) (m2,w2) = ((m1 * w1 + m2 * w2) / w, w) where w = w1 + w2

-------------------------------------------------------------------------------
-- Implementation
-------------------------------------------------------------------------------

emptyTDigest :: TDigest comp
emptyTDigest = TDigest 0 mempty 0 mempty True

combineTDigest :: forall comp. KnownNat comp => TDigest comp -> TDigest comp -> TDigest comp
combineTDigest (TDigest tw d _ b dir) (TDigest tw' d' _ b' dir') =
    TDigest (tw + tw') newD 0 [] (dir /= dir')
  where
    newD = VU.fromList
        . merge tw comp
        . sortBy (comparing fst)   -- sort
        $ VU.toList d ++ VU.toList d' ++ map (flip (,) 1) (b ++ b')

    comp = fromInteger (natVal (Proxy :: Proxy comp)) * sizeCoefficient

-- | Flush insertion buffer
finalize :: forall comp. KnownNat comp => TDigest comp -> TDigest comp
finalize td
    | null (tdigestBuffer td) = td
    | otherwise               = forceCompress td

forceCompress :: forall comp. KnownNat comp => TDigest comp -> TDigest comp
forceCompress (TDigest tw d _bs b dir) = TDigest tw d' 0 [] (not dir)
  where
    d' = VU.fromList
       . rev
       . merge tw comp            -- compress
       . rev
       . sortBy (comparing fst)   -- sort
       . (++ map (flip (,) 1) b)  -- add buffer
       . VU.toList
       $ d
    comp = fromInteger (natVal (Proxy :: Proxy comp)) * sizeCoefficient
    rev | dir       = id
        | otherwise = reverse

compress :: forall comp. KnownNat comp => TDigest comp -> TDigest comp
compress t@(TDigest _ _ bs _ _)
    | bs > compInt * 2 = forceCompress t
    | otherwise        = t
  where
    compInt = fromInteger (natVal (Proxy :: Proxy comp)) * sizeCoefficient

-------------------------------------------------------------------------------
-- Params
-------------------------------------------------------------------------------

sizeCoefficient :: Num a => a
sizeCoefficient = 32

-------------------------------------------------------------------------------
-- Debug
-------------------------------------------------------------------------------

-- | @'isRight' . 'validate'@
valid :: TDigest comp -> Bool
valid = isRight . validate

-- | Check various invariants in the 'TDigest' structure.
validate :: TDigest comp -> Either String (TDigest comp)
validate td@(TDigest tw d bs b _dir)
    | not (bs == length b) =
        Left $ "Buffer lenght don't match: " ++ show (bs, length b)
    | not (tw == bs + round dw) =
        Left "Total weight doesn't match"
    | dl /= sortBy (comparing fst) dl =
        Left "Data buffer isn't ordered"
    | otherwise = Right td
  where
    dl :: [Centroid]
    dl = VU.toList d

    -- total weight of @d@
    dw :: Double
    dw = sum (map snd dl)

-------------------------------------------------------------------------------
-- Higher level helpers
-------------------------------------------------------------------------------

-- | Insert single value into 'TDigest'.
insert
    :: KnownNat comp
    => Double  -- ^ element
    -> TDigest comp
    -> TDigest comp
insert x  = compress . insert' x

-- | Insert single value, don't compress 'TDigest' even if needed.
--
-- This may violate the insertion buffer size invariant.
--
-- For sensibly bounded input, it makes sense to let 'TDigest' grow (it might
-- grow linearly in size), and after that compress it once.
insert'
    :: KnownNat comp
    => Double         -- ^ element
    -> TDigest comp
    -> TDigest comp
insert' x (TDigest s d sb b dir) = TDigest (s + 1) d (sb + 1) (x : b) dir

-- | Make a 'TDigest' of a single data point.
singleton :: Double -> TDigest comp
singleton x = TDigest 1 (VU.singleton (x, 1)) 0 [] True

-- | Strict 'foldl'' over 'Foldable' structure.
tdigest :: (Foldable f, KnownNat comp) => f Double -> TDigest comp
tdigest = foldl' (flip insert) mempty . toList

-- $setup
-- >>> :set -XDataKinds
