{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Prelude ()
import Prelude.Compat
import Control.Monad               (join, replicateM, when)
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
    = MethodGuess
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
          MethodGuess           -> pure . guess
          MethodNaive           -> pure . naiveMedian
          MethodVector          -> pure . vectorMedian
          MethodTDigest         -> viaMachine c
          MethodTDigestBuffered -> viaBufferedMachine
          MethodTDigestSparking -> viaSparkingMachine
    timed $ method input

actionParser :: O.Parser (IO ())
actionParser = action
    <$> O.option (maybeReader readMethod) (
        O.short 'm' <> O.long "method" <> O.metavar ":method" <> O.value MethodTDigest)
    <*> O.option (maybeReader readDistrib) (
        O.short 'd' <> O.long "distrib" <> O.metavar ":distrib" <> O.value DistribUniform)
    <*> O.option O.auto (
        O.short 's' <> O.long "size" <> O.metavar ":size" <> O.value 1000000)
    <*> O.option O.auto (
        O.short 'c' <> O.long "compression" <> O.metavar ":comp" <> O.value 20)
  where
    readMethod "guess"    = Just MethodGuess
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

-- Returns the last value, but forces all in between.
guess :: [Double] -> Maybe Double
guess []     = Nothing
guess [x]    = Just x
guess (x:xs) = x `seq` guess xs

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

viaMachine :: Int -> [Double] -> IO (Maybe Double)
viaMachine compression input = case someNatVal (fromIntegral compression) of
    Nothing           -> fail "Negative compression"
    Just (SomeNat cp) -> do
        mdigest <- fmap validate <$> runT1 machine
        case mdigest of
            Nothing             -> return Nothing
            Just (Left err)     -> fail $ "Validation error: " ++ err
            Just (Right digest) -> do
                print $ median $ pd cp digest

                -- Extra: print quantiles
                for_ ([0.1,0.2..0.9] ++ [0.95,0.99,0.999,0.9999,0.99999]) $ \q ->
                    print (q, quantile q digest)

                return $ median digest
  where
    pd :: forall comp. Proxy comp -> TDigest comp -> TDigest comp
    pd _ x = x

    machine :: forall comp k. KnownNat comp => MachineT IO k (TDigest comp)
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
        =  fold mappend mempty
        <~ mapping (tdigest :: [Double] -> TDigest 10)
        <~ buffered 10000
        <~ autoM inputAction
        <~ counting
        <~ source input
    inputAction (x, i) = do
        when (i `mod` 1000000 == 0) $ putStrLn $ "consumed " ++ show i
        return x

-- Sparking machine doesn't count
viaSparkingMachine :: [Double] -> IO (Maybe Double)
viaSparkingMachine input = join . fmap median <$> runT1 machine
  where
    machine
        =  fold mappend mempty
        <~ sparking
        <~ mapping (tdigest :: [Double] -> TDigest 10)
        <~ buffered 10000
        <~ source input

-------------------------------------------------------------------------------
-- Machine additions
-------------------------------------------------------------------------------

sparking :: Process a a
sparking
    =  asParts
    <~ mapping (\x -> x `using` parList rseq)
    <~ buffered 10

counting :: Process a (a, Int)
counting = auto countingMealy
  where
    countingMealy = unfoldMealy (\i x -> ((x, i), i + 1)) 0

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
