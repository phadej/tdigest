{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
-- | Internals of 'TDigest'.
--
-- Tree implementation is based on /Adams’ Trees Revisited/ by Milan Straka
-- <http://fox.ucw.cz/papers/bbtree/bbtree.pdf>
module Data.TDigest.Tree.Internal where

import Control.DeepSeq        (NFData (..))
import Control.Monad.ST       (ST, runST)
import Data.Binary            (Binary (..))
import Data.Either            (isRight)
import Data.Foldable          (toList)
import Data.List.Compat       (foldl')
import Data.List.NonEmpty     (nonEmpty)
import Data.Ord               (comparing)
import Data.Proxy             (Proxy (..))
import Data.Semigroup         (Semigroup (..))
import Data.Semigroup.Reducer (Reducer (..))
import GHC.TypeLits           (KnownNat, Nat, natVal)
import Prelude ()
import Prelude.Compat

import qualified Data.Vector.Algorithms.Heap as VHeap
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as MVU

import           Data.TDigest.Internal
import qualified Data.TDigest.Postprocess.Internal as PP

-------------------------------------------------------------------------------
-- TDigest
-------------------------------------------------------------------------------

-- | 'TDigest' is a tree of centroids.
--
-- @compression@ is a @1/δ@. The greater the value of @compression@ the less
-- likely value merging will happen.
data TDigest (compression :: Nat)
    -- | Tree node
    = Node
        {-# UNPACK #-} !Size     -- size of this tree/centroid
        {-# UNPACK #-} !Mean     -- mean of the centroid
        {-# UNPACK #-} !Weight   -- weight of the centrod
        {-# UNPACK #-} !Weight   -- total weight of the tree
        !(TDigest compression)   -- left subtree
        !(TDigest compression)   -- right subtree
    -- | Empty tree
    | Nil
  deriving (Show)

-- [Note: keep min & max in the tree]
--
-- We tried it, but it seems the alloc/update cost is bigger than
-- re-calculating them on need (it's O(log n) - calculation!)

-- [Note: singleton node]
-- We tried to add one, but haven't seen change in performance

-- [Note: inlining balanceR and balanceL]
-- We probably can squueze some performance by making
-- 'balanceL' and 'balanceR' check arguments only once (like @containers@ do)
-- and not use 'node' function.
-- *But*, the benefit vs. code explosion is not yet worth.

instance KnownNat comp => Semigroup (TDigest comp) where
    (<>) = combineDigest

-- | Both 'cons' and 'snoc' are 'insert'
instance KnownNat comp => Reducer Double (TDigest comp) where
    cons = insert
    snoc = flip insert
    unit = singleton

instance  KnownNat comp => Monoid (TDigest comp) where
    mempty  = emptyTDigest
    mappend = combineDigest

-- | 'TDigest' has only strict fields.
instance NFData (TDigest comp) where
    rnf x = x `seq` ()

-- | 'TDigest' isn't compressed after de-serialisation,
-- but it can be still smaller.
instance KnownNat comp => Binary (TDigest comp) where
    put = put . getCentroids
    get = foldl' (flip insertCentroid) emptyTDigest . lc <$> get
      where
        lc :: [Centroid] -> [Centroid]
        lc = id

instance PP.HasHistogram (TDigest comp) Maybe where
    histogram = fmap PP.histogramFromCentroids . nonEmpty . getCentroids
    totalWeight = totalWeight

getCentroids :: TDigest comp -> [Centroid]
getCentroids = ($ []) . go
  where
    go Nil                = id
    go (Node _ x w _ l r) = go l . ((x,w) : ) . go r

-- | Total count of samples.
--
-- >>> totalWeight (tdigest [1..100] :: TDigest 5)
-- 100.0
--
totalWeight :: TDigest comp -> Weight
totalWeight Nil                 = 0
totalWeight (Node _ _ _ tw _ _) = tw

size :: TDigest comp -> Int
size Nil                    = 0
size (Node s _ _ _ _ _) = s

-- | Center of left-most centroid. Note: may be different than min element inserted.
--
-- >>> minimumValue (tdigest [1..100] :: TDigest 3)
-- 1.0
--
minimumValue :: TDigest comp -> Mean
minimumValue = go posInf
  where
    go  acc Nil                    = acc
    go _acc (Node _ x _ _ l _) = go x l

-- | Center of right-most centroid. Note: may be different than max element inserted.
--
-- >>> maximumValue (tdigest [1..100] :: TDigest 3)
-- 99.0
--
maximumValue :: TDigest comp -> Mean
maximumValue = go negInf
  where
    go  acc Nil                    = acc
    go _acc (Node _ x _ _ _ r) = go x r

-------------------------------------------------------------------------------
-- Implementation
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
combineDigest a@(Node n _ _ _ _ _) b@(Node m _ _ _ _ _)
    -- TODO: merge first, then shuffle and insert (part of compress)
    | n < m     = compress $ foldl' (flip insertCentroid) b (getCentroids a)
    | otherwise = compress $ foldl' (flip insertCentroid) a (getCentroids b)

insertCentroid
    :: forall comp. KnownNat comp
    => Centroid
    -> TDigest comp
    -> TDigest comp
insertCentroid (x, w) Nil        = singNode x w
insertCentroid (mean, weight) td = go 0 mean weight False td
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
        -> Bool          -- should insert everything.
                         -- if we merged somewhere on top, rest is inserted as is
        -> TDigest comp  -- subtree to insert/merge centroid into
        -> TDigest comp
    go _   newX newW _ Nil                 = singNode newX newW
    go cum newX newW e (Node s x w tw l r) = case compare newX x of
        -- Exact match, insert here
        EQ -> Node s x (w + newW) (tw + newW) l r -- node x (w + newW) l r

        -- there is *no* room to insert into this node
        LT | thr <= w -> balanceL x w (go cum newX newW e l) r
        GT | thr <= w -> balanceR x w l (go (cum + totalWeight l + w) newX newW e r)

        -- otherwise go left ... or later right
        LT | e -> balanceL x w (go cum newX newW e l) r
        LT -> case l of
            -- always create a new node
            Nil -> case mrw of
                Nothing     -> node' s nx nw (tw + newW) Nil r
                Just rw     -> balanceL nx nw (go cum newX rw True Nil) r
            Node _ _ _ _ _ _
                | lmax < newX && abs (newX - x) < abs (newX - lmax) {- && newX < x -} -> case mrw of
                    Nothing -> node' s nx nw (tw + nw - w) l r
                    -- in this two last LT cases, we have to recalculate size
                    Just rw -> balanceL nx nw (go cum newX rw True l) r
                | otherwise -> balanceL x w (go cum newX newW e l) r
              where
                lmax = maximumValue l

        -- ... or right
        GT | e -> balanceR x w l (go (cum + totalWeight l + w) newX newW True r)
        GT -> case r of
            Nil -> case mrw of
                Nothing     -> node' s nx nw (tw + newW) l Nil
                Just rw     -> balanceR nx nw l (go (cum + totalWeight l + nw) newX rw True Nil)
            Node _ _ _ _ _ _
                | rmin > newX && abs (newX - x) < abs (newX - rmin) {- && newX > x -} -> case mrw of
                    Nothing -> node' s nx nw (tw + newW) l r
                    -- in this two last GT cases, we have to recalculate size
                    Just rw -> balanceR nx nw l (go (cum + totalWeight l + nw) newX rw True r)
                | otherwise -> balanceR x w l (go (cum + totalWeight l + w) newX newW e r)
              where
                rmin = minimumValue r
      where
        -- quantile approximation of current node
        cum' = cum + totalWeight l
        q   = (w / 2 + cum') / n

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

-- | Constructor which calculates size and total weight.
node :: Mean -> Weight -> TDigest comp -> TDigest comp -> TDigest comp
node x w l r = Node
    (1 + size l + size r)
    x w
    (w + totalWeight l + totalWeight r)
    l r

-- | Balance after right insertion.
balanceR :: Mean -> Weight -> TDigest comp -> TDigest comp -> TDigest comp
balanceR x w l r
    | size l + size r <= 1 = node x w l r
    | size r > balOmega * size l = case r of
        Nil -> error "balanceR: impossible happened"
        (Node _ rx rw _ Nil rr) ->
            -- assert (0 < balAlpha * size rr) "balanceR" $
                -- single left rotation
                node rx rw (node x w l Nil) rr
        (Node _ rx rw _ rl rr)
            | size rl < balAlpha * size rr ->
                -- single left rotation
                node rx rw (node x w l rl) rr
        (Node _ rx rw _ (Node _ rlx rlw _ rll rlr) rr) ->
                -- double left rotation
                node rlx rlw (node x w l rll) (node rx rw rlr rr)
    | otherwise            = node x w l r

-- | Balance after left insertion.
balanceL :: Mean -> Weight -> TDigest comp -> TDigest comp -> TDigest comp
balanceL x w l r
    | size l + size r <= 1 = node x w l r
    | size l > balOmega * size r = case l of
        Nil -> error "balanceL: impossible happened"
        (Node _ lx lw _ ll Nil) ->
            -- assert (0 < balAlpha * size ll) "balanceL" $
                -- single right rotation
                node lx lw ll (node x w Nil r)
        (Node _ lx lw _ ll lr)
            | size lr < balAlpha * size ll ->
                -- single right rotation
                node lx lw ll (node x w lr r)
        (Node _ lx lw _ ll (Node _ lrx lrw _ lrl lrr)) ->
                -- double left rotation
                node lrx lrw (node lx lw ll lrl) (node x w lrr r)
    | otherwise = node x w l r

-- | Alias to 'Node'
node' :: Int -> Mean -> Weight -> Weight -> TDigest comp -> TDigest comp -> TDigest comp
node' = Node

-- | Create singular node.
singNode :: Mean -> Weight -> TDigest comp
singNode x w = Node 1 x w w Nil Nil

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

-------------------------------------------------------------------------------
-- Compression
-------------------------------------------------------------------------------

-- | Compress 'TDigest'.
--
-- Reinsert the centroids in "better" order (in original paper: in random)
-- so they have opportunity to merge.
--
-- Compression will happen only if size is both:
-- bigger than @'relMaxSize' * comp@ and bigger than 'absMaxSize'.
--
compress :: forall comp. KnownNat comp => TDigest comp -> TDigest comp
compress Nil = Nil
compress td
    | size td > relMaxSize * compression && size td > absMaxSize
        = forceCompress td
    | otherwise
        = td
  where
    compression = fromInteger $ natVal (Proxy :: Proxy comp)

-- | Perform compression, even if current size says it's not necessary.
forceCompress :: forall comp. KnownNat comp => TDigest comp -> TDigest comp
forceCompress Nil = Nil
forceCompress td =
    foldl' (flip insertCentroid) emptyTDigest $ fmap fst $ VU.toList centroids
  where
    -- Centroids are shuffled based on space
    centroids :: VU.Vector (Centroid, Double)
    centroids = runST $ do
        v <- toMVector td
        -- sort by cumulative weight
        VHeap.sortBy (comparing snd) v
        f <- VU.unsafeFreeze v
        pure f

toMVector
    :: forall comp s. KnownNat comp
    => TDigest comp                           -- ^ t-Digest
    -> ST s (VU.MVector s (Centroid, Double)) -- ^ return also a "space left in the centroid" value for "shuffling"
toMVector td = do
    v <- MVU.new (size td)
    (i, cum) <- go v (0 :: Int) (0 :: Double) td
    pure $ assert (i == size td && abs (cum - totalWeight td) < 1e-6) "traversal in toMVector:" v
  where
    go _ i cum Nil                   = pure (i, cum)
    go v i cum (Node _ x w _ l r) = do
        (i', cum') <- go v i cum l
        MVU.unsafeWrite v i' ((x, w), space w cum')
        go v (i' + 1) (cum' + w) r

    n = totalWeight td
    compression = fromInteger $ natVal (Proxy :: Proxy comp)

    space w cum = thr - w
      where
        q     = (w / 2 + cum) / n
        thr   = threshold n q compression

-------------------------------------------------------------------------------
-- Params
-------------------------------------------------------------------------------

-- | Relative size parameter. Hard-coded value: 25.
relMaxSize :: Int
relMaxSize = 25

-- | Absolute size parameter. Hard-coded value: 1000.
absMaxSize :: Int
absMaxSize = 1000

-------------------------------------------------------------------------------
-- Tree balance parameters
-------------------------------------------------------------------------------

balOmega :: Int
balOmega = 3

balAlpha :: Int
balAlpha = 2

-- balDelta = 0

-------------------------------------------------------------------------------
-- Debug
-------------------------------------------------------------------------------

-- | Output the 'TDigest' tree.
debugPrint :: TDigest comp -> IO ()
debugPrint td = go 0 td
  where
    go i Nil = putStrLn $ replicate (i * 3) ' ' ++ "Nil"
    go i (Node s m w tw l r) = do
        go (i + 1) l
        putStrLn $ replicate (i * 3) ' ' ++ "Node " ++ show (s,m,w,tw)
        go (i + 1) r

-- | @'isRight' . 'validate'@
valid :: TDigest comp -> Bool
valid = isRight . validate

-- | Check various invariants in the 'TDigest' tree.
validate :: TDigest comp -> Either String (TDigest comp)
validate td
    | not (all sizeValid   centroids) = Left "invalid sizes"
    | not (all weightValid centroids) = Left "invalid weights"
    | not (all orderValid  centroids) = Left "invalid ordering"
    | not (all balanced    centroids) = Left "tree is ill-balanced"
    | otherwise = Right td
  where
    centroids = goc td

    goc Nil = []
    goc n@(Node _ _ _ _ l r) = n : goc l ++ goc r

    sizeValid Nil = True
    sizeValid (Node s _ _ _ l r) = s == size l + size r + 1

    weightValid Nil = True
    weightValid (Node _ _ w tw l r) = eq tw $ w + totalWeight l + totalWeight r

    orderValid Nil = True
    orderValid (Node _ _ _ _ Nil                 Nil)                 = True
    orderValid (Node _ x _ _ (Node _ lx _ _ _ _) Nil)                 = lx < x
    orderValid (Node _ x _ _ Nil                 (Node _ rx _ _ _ _)) = x < rx
    orderValid (Node _ x _ _ (Node _ lx _ _ _ _) (Node _ rx _ _ _ _)) = lx < x && x < rx

    balanced Nil = True
    balanced (Node _ _ _ _ l r) =
        size l <= max 1 (balOmega * size r) &&
        size r <= max 1 (balOmega * size l)

-------------------------------------------------------------------------------
-- Higher level helpers
-------------------------------------------------------------------------------

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

-- | Make a 'TDigest' of a single data point.
singleton :: KnownNat comp => Double -> TDigest comp
singleton x = insert x emptyTDigest

-- | Strict 'foldl'' over 'Foldable' structure.
tdigest :: (Foldable f, KnownNat comp) => f Double -> TDigest comp
tdigest = foldl' insertChunk emptyTDigest . chunks . toList
  where
    -- compress after each chunk, forceCompress at the very end.
    insertChunk td xs =
        compress (foldl' (flip insert') td xs)

    chunks [] = []
    chunks xs =
        let (a, b) = splitAt 1000 xs -- 1000 is totally arbitrary.
        in a : chunks b

-- $setup
-- >>> :set -XDataKinds
