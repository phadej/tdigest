{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | One of reference impl for 'TDigest'.
--
-- Un-balanced tree.
module Data.TDigest.Internal.TreeRef where

import Prelude ()
import Prelude.Compat
import Data.List.Compat (foldl', sortOn)
import Data.Proxy       (Proxy (..))
import Data.Semigroup   (Semigroup (..))
import GHC.TypeLits     (KnownNat, Nat, natVal)

-- import Debug.Trace

assert :: Bool -> String -> a -> a
assert False msg _ = error msg
assert True  _   x = x

-------------------------------------------------------------------------------
-- TDigest
-------------------------------------------------------------------------------

-- TODO: make newtypes
type Mean = Double
type Weight = Double
type Centroid = (Mean, Weight)
type Size = Int

-- | 'TDigest' is a tree of centroids.
--
-- /TODO/ add singleton node?
data TDigest (compression :: Nat)
    -- | Tree node
    = Node
        {-# UNPACK #-} !Size     -- size of this tree/centroid
        {-# UNPACK #-} !Mean     -- mean of the centroid
        {-# UNPACK #-} !Weight   -- weight of the centrod
        {-# UNPACK #-} !Weight   -- total weight of the tree
        {-# UNPACK #-} !Mean     -- minimum value
        {-# UNPACK #-} !Mean     -- maximum value
        !(TDigest compression)   -- left subtree
        !(TDigest compression)   -- right subtree
    -- | Empty tree
    | Nil
  deriving (Show)

instance KnownNat comp => Semigroup (TDigest comp) where
    (<>) = combineDigest

instance  KnownNat comp => Monoid (TDigest comp) where
    mempty  = emptyTDigest
    mappend = combineDigest

getCentroids :: TDigest comp -> [Centroid]
getCentroids = ($ []) . go
  where
    go Nil                    = id
    go (Node _ x w _ _ _ l r) = go l . ((x,w) : ) . go r

totalWeight :: TDigest comp -> Double
totalWeight Nil                 = 0
totalWeight (Node _ _ _ tw _ _ _ _) = tw

size :: TDigest comp -> Int
size Nil                    = 0
size (Node s _ _ _ _ _ _ _) = s

minimumValue :: TDigest comp -> Mean
minimumValue Nil = posInf
minimumValue (Node _ _ _ _ m _ _ _) = m

maximumValue :: TDigest comp -> Mean
maximumValue Nil = negInf
maximumValue (Node _ _ _ _ _ m _ _) = m

-------------------------------------------------------------------------------
-- Impl
-------------------------------------------------------------------------------

emptyTDigest :: TDigest comp
emptyTDigest = Nil

combineDigest
    :: KnownNat comp
    => TDigest comp
    -> TDigest comp
    -> TDigest comp
combineDigest a Nil = a
combineDigest Nil b = b
combineDigest a@(Node n _ _ _ _ _ _ _) b@(Node m _ _ _ _ _ _ _)
    -- TODO: merge first, then shuffle and insert (part of compress)
    | n < m     = compress $ foldl' (flip insertCentroid) b (getCentroids a)
    | otherwise = compress $ foldl' (flip insertCentroid) a (getCentroids b)

insertCentroid
    :: forall comp. KnownNat comp
    => Centroid
    -> TDigest comp
    -> TDigest comp
insertCentroid (x, w) Nil                     = singNode x w
insertCentroid (mean, weight) td = go 0 mean weight td
  where
    -- New weight of the tree
    n :: Weight
    n = totalWeight td + weight

    -- 1/delta
    compression :: Double
    compression = fromInteger $ natVal (Proxy :: Proxy comp)

    go
        :: Weight        -- weight to the left of this tree
        -> Mean          -- mean to insert
        -> Weight        -- weight to insert
        -> TDigest comp  -- subtree to insert/merge centroid into
        -> TDigest comp
    go _   newX newW Nil                    = singNode newX newW
    go cum newX newW (Node _ x w _ _ _ l r) = case compare newX x of
        -- Exact match, insert here
        EQ -> node x (w + newW) l r

        -- there is *no* room to insert into this node
        LT | thr < w -> node x w (go cum newX newW l) r
        GT | thr < w -> node x w l (go (cum + totalWeight l + w) newX newW r)

        -- otherwise go left ... or later right
        LT -> case l of
            Nil -> case mrw of
                Nothing     -> node nx nw l r
                Just rw     -> node nx nw (go cum newX rw l) r
            Node _ _ _ _ _ lmax _ _
                | lmax < newX && abs (newX - x) < abs (newX - lmax) {- && newX < x -} -> case mrw of
                    Nothing -> node nx nw l r
                    Just rw -> node nx nw (go cum newX rw l) r
                | otherwise -> node x w (go cum newX newW l) r

        -- ... or right
        GT -> case r of
            Nil -> case mrw of
                Nothing     -> node nx nw l r
                Just rw     -> node nx nw l (go (cum + totalWeight l + nw) newX rw r)
            Node _ _ _ _ lmin _ _ _
                | lmin > newX && abs (newX - x) < abs (newX - lmin) {- && newX > x -} -> case mrw of
                    Nothing -> node nx nw l r
                    Just rw -> node nx nw l (go (cum + totalWeight l + nw) newX rw r)
                | otherwise -> node x w l (go (cum + totalWeight l + w) newX newW r)
      where
        -- quantile approximation of current node
        q   = (w / 2 + cum) / n

        -- threshold, max size of current node/centroid
        thr = {- traceShowId $ traceShow (n, q) $ -} threshold n q compression

        -- We later use nx, nw and mrw:

        -- max size of current node
        dw :: Weight
        mrw :: Maybe Weight
        (dw, mrw) =
            let diff = assert (thr > w) "threshold should be larger than current node weight"
                     $ w + newW - thr
            in if diff < 0 -- i.e. there is room
                then (newW, Nothing)
                else (thr - w, Just $ diff)

        -- the change of current node
        (nx, nw) = {- traceShowId $ traceShow (newX, newW, x, dw, mrw) $ -} combinedCentroid x w x dw

node :: Mean -> Weight -> TDigest comp -> TDigest comp -> TDigest comp
node x w l r = Node
    (1 + size l + size r)
    x w
    (w + totalWeight l + totalWeight r)
    (min x (minimumValue l))
    (max x (maximumValue r))
    l r

-- | TODO: temp
singNode :: Mean -> Weight -> TDigest comp
singNode x w = Node 1 x w w x x Nil Nil

negInf :: Double
negInf = negate posInf

posInf :: Double
posInf = 1/0

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
compress Nil = Nil
compress td
    | fromIntegral (size td) > konst * compression
        = foldl' (flip insertCentroid) emptyTDigest
        $ shuffle (totalWeight td) compression
        $ getCentroids td
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
    . sortOn snd
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

valid :: TDigest comp -> Bool
valid _ = True

validate :: TDigest comp -> TDigest comp
validate td
    | not (all sizeValid centroids)   = error "invalid sizes"
    | not (all weightValid centroids) = error "invalid weights"
    | not (all orderValid centroids)  = error "invalid ordering"
    | not (all maxValid centroids)    = error "invalid maximum value"
    | otherwise = td
  where
    centroids = goc td

    goc Nil = []
    goc n@(Node _ _ _ _ _ _ l r) = n : goc l ++ goc r

    sizeValid Nil = True
    sizeValid (Node s _ _ _ _ _ l r) = s == size l + size r + 1

    weightValid Nil = True
    weightValid (Node _ _ w tw _ _ l r) = tw == w + totalWeight l + totalWeight r

    orderValid Nil = True
    orderValid (Node _ _ _ _ _ _ Nil Nil) = True
    orderValid (Node _ x _ _ _ _ (Node _ l _ _ _ _ _ _) Nil) = l < x
    orderValid (Node _ x _ _ _ _ Nil (Node _ r _ _ _ _ _ _) ) = x < r
    orderValid (Node _ x _ _ _ _ (Node _ l _ _ _ _ _ _) (Node _ r _ _ _ _ _ _) ) = l < x && l < r

    maxValid Nil = True
    maxValid (Node _ x _ _ _ m _ r) = m == max x (maximumValue r)



{-
validate td@(TDigest centroids n)
    | abs (sum centroids - n) >= 0.001
        = error $ "Sum don't match: " ++ show (n, sum centroids)
    | otherwise = td
-}
