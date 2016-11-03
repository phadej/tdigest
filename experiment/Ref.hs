{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Internals of 'TDigest'. This is "reference", not so efficient implementation
-- based on "Data.Map.Strict" from @containers@.
module Data.TDigest.Internal.Ref where

import Prelude ()
import Prelude.Compat
import Data.List.Compat (foldl', sortBy, sortOn)
import Data.Maybe       (maybeToList)
import Data.Ord         (comparing)
import Data.Proxy       (Proxy (..))
import Data.Semigroup   (Semigroup (..))
import GHC.TypeLits     (KnownNat, Nat, natVal)

import qualified Data.Map.Strict as Map

-------------------------------------------------------------------------------
-- TDigest
-------------------------------------------------------------------------------

-- TODO: make newtypes
type Mean = Double
type Weight = Double
type Centroid = (Mean, Weight)

data TDigest (compression :: Nat) = TDigest
    { tdCentroids :: !(Map.Map Mean Weight)
    , tdCount     :: !Weight
    }
  deriving (Show)

instance KnownNat comp => Semigroup (TDigest comp) where
    (<>) = combineDigest

instance  KnownNat comp => Monoid (TDigest comp) where
    mempty  = emptyTDigest
    mappend = combineDigest

getCentroids :: TDigest comp -> [Centroid]
getCentroids = Map.toList . tdCentroids

totalWeight :: TDigest comp -> Double
totalWeight = tdCount

-------------------------------------------------------------------------------
-- Impl
-------------------------------------------------------------------------------

emptyTDigest :: TDigest comp
emptyTDigest = TDigest Map.empty 0

combineDigest
    :: KnownNat comp
    => TDigest comp
    -> TDigest comp
    -> TDigest comp
combineDigest (TDigest acs an) (TDigest bcs bn) =
    compress $ TDigest (Map.unionWith (+) acs bcs) (an + bn)

insertCentroid
    :: forall comp. KnownNat comp
    => Centroid
    -> TDigest comp
    -> TDigest comp
insertCentroid (x, w) (TDigest centroids count)
    -- x in map, special case
    | x `Map.member` centroids = TDigest (Map.adjust (+w) x centroids) n
    | otherwise                = validate $ TDigest (process n compression centroids' x w s2) n
  where
    n = count + w

    le = Map.lookupLE x centroids
    ge = Map.lookupGE x centroids

    s0 = maybeToList le ++ maybeToList ge
    s1 = sortOn (\(y, _) -> abs (y - x)) s0
    s2 = take 2 s1

    -- centrois without keys in s0
    centroids' = foldr Map.delete centroids (map fst s2)

    compression = fromInteger $ natVal (Proxy :: Proxy comp)

-- | Insert of value, which doesn't have exact centroid in 'TDigest'.
process
    :: Double               -- ^ total weight
    -> Double               -- ^ compression
    -> Map.Map Mean Weight  -- ^ centroids
    -> Mean                 -- ^ value/mean
    -> Weight               -- ^ weight to add
    -> [Centroid]           -- ^ potential centroids to merge with
    -> Map.Map Mean Weight  -- ^ result is updated centroids
process _ _ cs x w []
    | not (w > 0) = cs
    -- no potential centroids anymore, add new one
    | otherwise = Map.insert x w cs

process n compression cs x w s0@((x', w') : s1)
    -- If weight of potential centroids is way bigger than it's bucket size
    -- just reinsert it, and continue
    | w' > thr      = process n compression (Map.insert x' w' cs) x w s1
    -- If there nothing to add anymore
    | not (w > 0)   = foldr insertPair cs s0
    -- Otherwise adjust centroid, and proceed with the rest
    -- - | abs ((w + w') - (snd nc + (w - dw))) > 0.001 = error $ show (x, w, w + w', snd nc + (w - dw))
    | otherwise     = process n compression (insertPair nc cs) x (w - dw) s1
  where
    cum = sum $ map snd $ takeWhile ((< x') . fst) $ Map.toList cs
    q   = (w' / 2 + cum) / n
    thr = threshold n q compression
    dw  = min (thr - w') w
    nc  = combinedCentroid x' w' x dw

-- | Add two weighted means together.
combinedCentroid
    :: Mean -> Weight
    -> Mean -> Weight
    -> Centroid
combinedCentroid x w x' w' =
    ( (x * w + x' * w') / w'' -- this is probably not num. stable
    , w''
    )
  where
    w'' = w + w'

-- | Calculate the threshold, i.e. maximum weight of centroid.
threshold
    :: Double  -- ^ total weight
    -> Double  -- ^ quantile
    -> Double  -- ^ compression (1/δ)
    -> Double
threshold n q compression = 4 * n * q * (1 - q) / compression

compress :: forall comp. KnownNat comp => TDigest comp -> TDigest comp
compress td@(TDigest centroids n)
    | fromIntegral (length centroids) > konst * compression
        = foldl' (flip insertCentroid) emptyTDigest
        $ shuffle n compression
        $ Map.toList centroids
    | otherwise =
         td
  where
    compression = fromInteger $ natVal (Proxy :: Proxy comp)

-- | Actually not a shuffle, but deterministic ordering,
-- ordering most full centroids first.
--
-- For some reason this approach seems to work.
shuffle
    :: Double -- ^ Total weight
    -> Double -- ^ compression
    -> [Centroid]
    -> [Centroid]
shuffle n compression
    = map fst
    . sortBy (comparing snd)
    . calc 0
  where
    calc _ [] = []
    calc cum (c@(_, w) : cs) = (c, space) : calc (cum + w) cs
      where
        q     = (w / 2 + cum) / n
        thr   = threshold n q compression
        space = thr - w

-- | Size parameter, /K/. Hard-coded value: 25.
konst :: Double
konst = 25

-------------------------------------------------------------------------------
-- Debug
-------------------------------------------------------------------------------

validate :: TDigest comp -> TDigest comp
validate = id

{-
validate td@(TDigest centroids n)
    | abs (sum centroids - n) >= 0.001
        = error $ "Sum don't match: " ++ show (n, sum centroids)
    | otherwise = td
-}

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

-- | Insert pair into 'Data.Map.Map'
insertPair :: Ord k => (k, v) -> Map.Map k v -> Map.Map k v
insertPair (k, v) = Map.insert k v
