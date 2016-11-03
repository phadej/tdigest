{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Control.Category            (Category)
import Control.Monad               (join, replicateM, when)
import Control.Monad.ST            (runST)
import Control.Parallel.Strategies (parList, rseq, using)
import Data.Foldable               (traverse_)
import Data.List                   (sort)
import Data.Machine
import Data.Machine.Runner         (runT1)
import Data.Semigroup              ((<>))
import Data.Time
import Data.Word                   (Word32)
import Statistics.Distribution     (ContGen (..))

import Statistics.Distribution.Exponential (exponential)
import Statistics.Distribution.Uniform     (uniformDistr)

import qualified Data.Vector.Algorithms.Intro as Intro
import qualified Data.Vector.Unboxed          as V
import qualified Data.Vector.Unboxed.Mutable  as VU
import qualified Options.Applicative          as O
import qualified System.Random.MWC            as MWC

import Data.TDigest


-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

data Method
    = MethodNaive
    | MethodVector
    | MethodTDigest
    | MethodTDigestBuffered
    | MethodTDigestSparking
  deriving (Show)

data Distrib
    = DistribIncr
    | DistribUniform
    | DistribExponent
  deriving (Show)

timed :: Show a => IO a -> IO ()
timed mx = do
    s <- getCurrentTime
    x <- mx
    print x
    e <- getCurrentTime
    print (diffUTCTime e s)

action :: Method -> Distrib -> Int -> IO ()
action m d s = do
    print (m, d, s)
    let seed = initSeed (V.singleton 42)
    let input = take s $ case d of
            DistribIncr     -> [1 .. fromIntegral s] -- not sure, but end point prevents floating
            DistribUniform  -> randomStream (uniformDistr 0 1) seed     -- median around 0.5
            DistribExponent -> randomStream (exponential $ log 2) seed  -- median around 1.0
    let method = case m of
          MethodNaive           -> pure . naiveMedian
          MethodVector          -> pure . vectorMedian
          MethodTDigest         -> viaMachine
          MethodTDigestBuffered -> viaBufferedMachine
          MethodTDigestSparking -> viaSparkingMachine
    timed $ method input

actionParser :: O.Parser (IO ())
actionParser = action
    <$> O.option (O.maybeReader readMethod) (
        O.short 'm' <> O.long "method" <> O.metavar ":method" <> O.value MethodTDigest)
    <*> O.option (O.maybeReader readDistrib) (
        O.short 'd' <> O.long "distrib" <> O.metavar ":distrib" <> O.value DistribUniform)
    <*> O.option O.auto (
        O.short 's' <> O.long "size" <> O.metavar ":size" <> O.value 1000000)
  where
    readMethod "naive"    = Just MethodNaive
    readMethod "vector"   = Just MethodVector
    readMethod "digest"   = Just MethodTDigest
    readMethod "buffered" = Just MethodTDigestBuffered
    readMethod "sparking" = Just MethodTDigestSparking
    readMethod _          = Nothing

    readDistrib "incr"     = Just DistribIncr
    readDistrib "uniform"  = Just DistribUniform
    readDistrib "exponent" = Just DistribExponent
    readDistrib _          = Nothing

main :: IO ()
main = join (O.execParser opts)
  where
    opts = O.info (O.helper <*> actionParser)
        (O.fullDesc <> O.header "tdigest-simple - a small utility to explore tdigest")

-------------------------------------------------------------------------------
-- Methods
-------------------------------------------------------------------------------

naiveMedian :: [Double] -> Maybe Double
naiveMedian [] = Nothing
naiveMedian xs = Just $ sort xs !! (length xs `div` 2)

vectorMedian :: [Double] -> Maybe Double
vectorMedian l
    | null l    = Nothing
    | otherwise = runST $ do
        let v = V.fromList l
        mv <- V.thaw v
        Intro.sort mv
        Just <$> VU.unsafeRead mv (VU.length mv `div` 2)

viaMachine :: [Double] -> IO (Maybe Double)
viaMachine input = join . fmap (median :: TDigest 10 -> Maybe Double) <$> runT1 machine
  where
    machine
        =  fold (flip insert) mempty
        <~ autoM inputAction
        <~ counting
        <~ source input
    inputAction (x, i) = do
        when (i `mod` 1000000 == 0) $ putStrLn $ "consumed " ++ show i
        return x

viaBufferedMachine :: [Double] -> IO (Maybe Double)
viaBufferedMachine input = join . fmap median <$> runT1 machine
  where
    machine
        = fold mappend mempty
        <~ mapping (tdigest :: [Double] -> TDigest 10)
        <~ buffered 10000
        <~ autoM inputAction
        <~ counting
        <~ source input
    inputAction (x, i) = do
        when (i `mod` 1000000 == 0) $ putStrLn $ "consumed " ++ show i
        return x

viaSparkingMachine :: [Double] -> IO (Maybe Double)
viaSparkingMachine input = join . fmap median <$> runT1 machine
  where
    machine
        =  fold mappend mempty
        <~ sparking
        <~ mapping (tdigest :: [Double] -> TDigest 10)
        <~ buffered 10000
        <~ autoM inputAction
        <~ counting
        <~ source input
    inputAction (x, i) = do
        when (i `mod` 1000000 == 0) $ putStrLn $ "consumed " ++ show i
        return x

-------------------------------------------------------------------------------
-- Machine additions
-------------------------------------------------------------------------------

sparking :: (Monad m) => ProcessT m a a
sparking
    =  asParts
    <~ mapping (\x -> x `using` parList rseq)
    <~ buffered 10

counting :: Monad m => ProcessT m a (a, Int)
counting = myscan f 0
  where
    f n x = (n + 1, (x, n))

myscan :: (Category k, Monad m) => (s -> b -> (s, a)) -> s -> MachineT m (k b) a
myscan func seed = construct $ go seed
  where
    go s = do
        next <- await
        let (s', x) = func s next
        yield x
        go $! s'

-------------------------------------------------------------------------------
-- Statistics additions
-------------------------------------------------------------------------------

randomStream :: ContGen d => d -> MWC.Seed -> [Double]
randomStream d = go
  where
    continue (xs, seed) = xs ++ go seed
    go seed = continue $ runST $ do
        g <- MWC.restore seed
        -- Generate first 10000 elements
        xs <- replicateM 10000 (genContVar d g)
        seed' <- MWC.save g
        pure (xs, seed')

initSeed :: V.Vector Word32 -> MWC.Seed
initSeed v = runST $ MWC.initialize v >>= MWC.save
