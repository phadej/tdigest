{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.TDigest.Internal.Type where

import Prelude ()
import Prelude.Compat
import Data.List      (foldl', sortBy, sortOn)
import Data.Maybe     (maybeToList)
import Data.Ord       (comparing)
import Data.Proxy     (Proxy (..))
import Data.Semigroup (Semigroup (..))
import GHC.TypeLits   (KnownNat, Nat, natVal)

import qualified Data.Map as Map

-------------------------------------------------------------------------------
-- TDigest
-------------------------------------------------------------------------------

-- TODO: make newtypes
type Mean = Double
type Weight = Double

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

-------------------------------------------------------------------------------
-- Impl
-------------------------------------------------------------------------------

type Centroid = (Mean, Weight)

emptyTDigest :: TDigest comp
emptyTDigest = TDigest Map.empty 0

combineDigest
    :: KnownNat comp
    => TDigest comp
    -> TDigest comp
    -> TDigest comp
combineDigest (TDigest acs an) (TDigest bcs bn) =
    compress $ TDigest (Map.unionWith (+) acs bcs) (an + bn)

insert
    :: KnownNat comp
    => Double         -- ^ element
    -> TDigest comp
    -> TDigest comp
insert x = insertCentroid (x, 1)

insertCentroid
    :: forall comp. KnownNat comp
    => Centroid
    -> TDigest comp
    -> TDigest comp
insertCentroid (x, w) (TDigest centroids count)
    -- x in map, special case
    | x `Map.member` centroids = TDigest (Map.adjust (+w) x centroids) n
    | otherwise                = compress $ validate $ TDigest (process n compression centroids' x w s2) n
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

process
    :: Double               -- Total weight
    -> Double
    -> Map.Map Mean Weight  -- centroids
    -> Mean                 -- mean
    -> Weight               -- weight to add
    -> [Centroid]           -- potential centroids
    -> Map.Map Mean Weight  -- result is updated centroids
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
    | abs ((w + w') - (snd nc + (w - dw))) > 0.001 = error $ show (x, w, w + w', snd nc + (w - dw))
    | otherwise     = process n compression (insertPair nc cs) x (w - dw) s1
  where
    cum = sum $ map snd $ takeWhile ((< x') . fst) $ Map.toList cs
    q   = (w' / 2 + cum) / n
    thr = threshold n q compression
    dw  = min (thr - w') w
    nc  = combinedCentroid x' w' x dw

notEq :: Double -> Double -> Bool
notEq a b = abs (a - b) > 0.0001

insertPair :: Ord k => (k, v) -> Map.Map k v -> Map.Map k v
insertPair (k, v) = Map.insert k v

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

threshold :: Double -> Double -> Double -> Double
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

-- Actually not a shuffle, but deterministic ordering
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

