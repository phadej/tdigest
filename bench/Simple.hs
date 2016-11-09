{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Prelude ()
import Prelude.Compat
import Control.Monad               (join, replicateM)
import Control.Monad.ST            (runST)
import Control.Parallel.Strategies (parList, rseq, using)
import Data.Foldable               (for_)
import Data.List                   (sort)
import Data.Machine
import Data.Machine.Runner         (runT1)
import Data.Monoid                 ((<>))
import Data.Proxy                  (Proxy (..))
import Data.Time                   (diffUTCTime, getCurrentTime)
import Data.Word                   (Word32)
import GHC.TypeLits                (KnownNat, SomeNat (..), someNatVal)
import Statistics.Distribution     (ContGen (..))

import Statistics.Distribution.Exponential (exponential)
import Statistics.Distribution.Gamma       (gammaDistr)
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
    = MethodAverage
    | MethodNaive
    | MethodVector
    | MethodTDigest
    | MethodTDigestBuffered
    | MethodTDigestSparking
  deriving (Show)

data Distrib
    = DistribIncr
    | DistribUniform
    | DistribExponent
    | DistribGamma
  deriving (Show)

timed :: Show a => IO a -> IO ()
timed mx = do
    s <- getCurrentTime
    x <- mx
    print x
    e <- getCurrentTime
    print (diffUTCTime e s)

action :: Method -> Distrib -> Int -> Int -> IO ()
action m d s c = do
    print (m, d, s, c)
    let seed = initSeed (V.singleton 42)
    let input = take s $ case d of
            DistribIncr     -> [1 .. fromIntegral s] -- not sure, but end point prevents floating
            DistribUniform  -> randomStream (uniformDistr 0 1) seed     -- median around 0.5
            DistribExponent -> randomStream (exponential $ log 2) seed  -- median around 1.0
            DistribGamma    -> randomStream (gammaDistr 0.1 0.1) seed    -- median around .0000593391
    let method = case m of
          MethodAverage         -> pure . average
          MethodNaive           -> pure . naiveMedian
          MethodVector          -> pure . vectorMedian
          MethodTDigest         -> reifyNat c tdigestMachine
          MethodTDigestBuffered -> reifyNat c tdigestBufferedMachine
          MethodTDigestSparking -> reifyNat c tdigestSparkingMachine
    timed $ method input

reifyNat :: forall x. Int -> (forall n. KnownNat n => Proxy n -> x) -> x
reifyNat n f = case someNatVal (fromIntegral n) of
    Nothing           -> error "Negative m"
    Just (SomeNat cp) -> f cp

actionParser :: O.Parser (IO ())
actionParser = action
    <$> O.option (maybeReader readMethod) (
        O.short 'm' <> O.long "method" <> O.metavar ":method" <> O.value MethodTDigestBuffered)
    <*> O.option (maybeReader readDistrib) (
        O.short 'd' <> O.long "distrib" <> O.metavar ":distrib" <> O.value DistribUniform)
    <*> O.option O.auto (
        O.short 's' <> O.long "size" <> O.metavar ":size" <> O.value 1000000)
    <*> O.option O.auto (
        O.short 'c' <> O.long "compression" <> O.metavar ":comp" <> O.value 20)
  where
    readMethod "average"    = Just MethodAverage
    readMethod "naive"    = Just MethodNaive
    readMethod "vector"   = Just MethodVector
    readMethod "digest"   = Just MethodTDigest
    readMethod "buffered" = Just MethodTDigestBuffered
    readMethod "sparking" = Just MethodTDigestSparking
    readMethod _          = Nothing

    readDistrib "incr"     = Just DistribIncr
    readDistrib "uniform"  = Just DistribUniform
    readDistrib "exponent" = Just DistribExponent
    readDistrib "gamma"    = Just DistribGamma
    readDistrib _          = Nothing

-- Only on optparse-applicative-0.13
maybeReader :: (String -> Maybe a) -> O.ReadM a
maybeReader f = O.eitherReader $ \x -> maybe (Left x) Right (f x)

main :: IO ()
main = join (O.execParser opts)
  where
    opts = O.info (O.helper <*> actionParser)
        (O.fullDesc <> O.header "tdigest-simple - a small utility to explore tdigest")

-------------------------------------------------------------------------------
-- Methods
-------------------------------------------------------------------------------

average :: [Double] -> Maybe Double
average []     = Nothing
average (x:xs) = Just $ go x 1 xs
  where
    go z _ []       = z
    go z n (y : ys) = go ((z * n + y) / (n + 1)) (n + 1) ys

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

tdigestMachine :: forall comp. KnownNat comp => Proxy comp -> [Double] -> IO (Maybe Double)
tdigestMachine _ input = do
    mdigest <- fmap validate <$> runT1 machine
    case mdigest of
        Nothing             -> return Nothing
        Just (Left err)     -> fail $ "Validation error: " ++ err
        Just (Right digest) -> do
            printStats digest
            return $ median digest
  where
    machine :: MachineT IO k (TDigest comp)
    machine
        =  fold (flip insert) mempty
        <~ source input

tdigestBufferedMachine :: forall comp. KnownNat comp => Proxy comp -> [Double] -> IO (Maybe Double)
tdigestBufferedMachine _ input = do
    mdigest <- fmap validate <$> runT1 machine
    case mdigest of
        Nothing             -> return Nothing
        Just (Left err)     -> fail $ "Validation error: " ++ err
        Just (Right digest) -> do
            printStats digest
            return $ median digest
  where
    machine :: MachineT IO k (TDigest comp)
    machine
        =  fold mappend mempty
        <~ mapping tdigest
        <~ buffered 10000
        <~ source input

-- Sparking machine doesn't count
tdigestSparkingMachine :: forall comp. KnownNat comp => Proxy comp -> [Double] -> IO (Maybe Double)
tdigestSparkingMachine _ input = do
    mdigest <- fmap validate <$> runT1 machine
    case mdigest of
        Nothing             -> return Nothing
        Just (Left err)     -> fail $ "Validation error: " ++ err
        Just (Right digest) -> do
            printStats digest
            return $ median digest
  where
    machine :: MachineT IO k (TDigest comp)
    machine
        =  fold mappend mempty
        <~ sparking
        <~ mapping tdigest
        <~ buffered 10000
        <~ source input

printStats :: TDigest comp -> IO ()
printStats digest = do
    -- Extra: print quantiles
    putStrLn "quantiles"
    for_ ([0.1,0.2..0.9] ++ [0.95,0.99,0.999,0.9999,0.99999]) $ \q ->
        putStrLn $ show q ++ ":" ++ show (quantile q digest)
    putStrLn "cdf"
    for_ ([0, 0.25, 0.5, 1, 2]) $ \x ->
        putStrLn $ show x ++ ": " ++ show (cdf x digest)

-------------------------------------------------------------------------------
-- Machine additions
-------------------------------------------------------------------------------

sparking :: Process a a
sparking
    =  asParts
    <~ mapping (\x -> x `using` parList rseq)
    <~ buffered 10

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
