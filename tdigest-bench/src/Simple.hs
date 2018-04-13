{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Main (main, tdigestBinSize) where

import Control.Monad               (join, replicateM)
import Control.Monad.ST            (runST)
import Control.Parallel.Strategies (parList, rseq, using)
import Data.Foldable               (for_, toList)
import Data.List                   (foldl', sort)
import Data.Machine
import Data.Machine.Runner         (runT1)
import Data.Proxy                  (Proxy (..))
import Data.Semigroup              (Semigroup (..))
import Data.Semigroup.Reducer      (Reducer (..))
import Data.Time                   (diffUTCTime, getCurrentTime)
import Data.Word                   (Word32)
import GHC.TypeLits                (KnownNat, SomeNat (..), natVal, someNatVal)
import Numeric                     (showFFloat)
import Prelude ()
import Prelude.Compat
import Statistics.Distribution
       (ContDistr (..), ContGen (..), Mean (..), Variance (..), cumulative)

import Statistics.Distribution.Exponential (exponential)
import Statistics.Distribution.Gamma       (gammaDistr)
import Statistics.Distribution.Normal      (standard)
import Statistics.Distribution.Uniform     (uniformDistr)

import qualified Data.Vector.Algorithms.Intro as Intro
import qualified Data.Vector.Unboxed          as V
import qualified Data.Vector.Unboxed.Mutable  as VU
import qualified Options.Applicative          as O
import qualified System.Random.MWC            as MWC

import qualified Graphics.Rendering.Chart.Backend.Diagrams as Chart
import qualified Graphics.Rendering.Chart.Easy             as Chart
import qualified Graphics.Rendering.Chart.Plot.TDigest     as Chart

import           Data.TDigest.Postprocess as PP
import qualified Data.TDigest.Tree        as TDT
import qualified Data.TDigest.Vector      as TDV

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

data Method
    = MethodAverage
    | MethodAverageMachine
    | MethodAverageSparking
    | MethodNaive
    | MethodVector
    | MethodTDigest
    | MethodTDigestDirect
    | MethodTDigestBuffered
    | MethodTDigestSparking

    | MethodTDigestVecDirect
  deriving (Show)

data Distrib
    = DistribIncr
    | DistribUniform
    | DistribExponent
    | DistribGamma
    | DistribStandard
  deriving (Show)

data SomeContDistr where
    SomeContDistr :: (Mean d, Variance d, ContDistr d) => d -> SomeContDistr

timed :: Show a => IO a -> IO ()
timed mx = do
    s <- getCurrentTime
    x <- mx
    putStrLn $ "Result: " ++ show x
    e <- getCurrentTime
    print (diffUTCTime e s)

action :: Method -> Distrib -> Int -> Int -> Word32 -> Maybe FilePath -> IO ()
action m d s c iseed fp = do
    print (m, d, s, c)
    let seed = initSeed (V.singleton iseed)
    let distr = case d of
            DistribIncr     -> SomeContDistr $ uniformDistr 0 (fromIntegral s)
            DistribUniform  -> SomeContDistr $ uniformDistr 0 1     -- median around 0.5
            DistribExponent -> SomeContDistr $ exponential $ log 2  -- median around 1.0
            DistribGamma    -> SomeContDistr $ gammaDistr 0.1 0.1   -- median around .0000593391
            DistribStandard -> SomeContDistr standard
    let input = take s $ case d of
            DistribIncr     -> [1 .. fromIntegral s] -- not sure, but end point prevents floating
            DistribUniform  -> randomStream (uniformDistr 0 1) seed     -- median around 0.5
            DistribExponent -> randomStream (exponential $ log 2) seed  -- median around 1.0
            DistribGamma    -> randomStream (gammaDistr 0.1 0.1) seed   -- median around .0000593391
            DistribStandard -> randomStream standard seed
    let method = case m of
          MethodAverage          -> pure . listAverage
          MethodAverageMachine   -> reducerMachine getAverage
          MethodAverageSparking  -> reducerSparkingMachine getAverage
          MethodNaive            -> pure . naiveMedian
          MethodVector           -> pure . vectorMedian
          MethodTDigest          -> reifyNat c $ tdigestMachine fp distr
          MethodTDigestDirect    -> reifyNat c $ tdigestDirect fp distr
          MethodTDigestBuffered  -> reifyNat c $ tdigestBufferedMachine fp distr
          MethodTDigestSparking  -> reifyNat c $ tdigestSparkingMachine fp distr

          MethodTDigestVecDirect -> reifyNat c $ tdigestVecDirect fp distr
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
    <*> O.option O.auto (
        O.short 'i' <> O.long "seed" <> O.metavar ":seed" <> O.value 42)
    <*> O.optional (O.strOption (
        O.short 'o' <> O.long "output" <> O.metavar ":output.svg"))
  where
    readMethod "average"    = Just MethodAverage
    readMethod "averagem"   = Just MethodAverageMachine
    readMethod "averages"   = Just MethodAverageSparking
    readMethod "naive"      = Just MethodNaive
    readMethod "vector"     = Just MethodVector
    readMethod "digest"     = Just MethodTDigest
    readMethod "tdigest"    = Just MethodTDigest
    readMethod "direct"     = Just MethodTDigestDirect
    readMethod "buffered"   = Just MethodTDigestBuffered
    readMethod "sparking"   = Just MethodTDigestSparking
    readMethod "vec-direct" = Just MethodTDigestVecDirect
    readMethod _            = Nothing

    readDistrib "incr"     = Just DistribIncr
    readDistrib "uniform"  = Just DistribUniform
    readDistrib "exponent" = Just DistribExponent
    readDistrib "standard" = Just DistribStandard
    readDistrib "normal"   = Just DistribStandard
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

listAverage :: [Double] -> Maybe Double
listAverage []     = Nothing
listAverage (x:xs) = Just $ go x 1 xs
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

reducerMachine
    :: forall m i. (Reducer i m, Monoid m)
    => (m -> Double) -> [i] -> IO (Maybe Double)
reducerMachine f input = do
    x <- runT1 machine
    return (fmap f x)
  where
    machine :: MachineT IO k m
    machine
        =  fold snoc mempty
        <~ source input

reducerSparkingMachine
    :: forall m i. (Reducer i m, Monoid m)
    => (m -> Double) -> [i] -> IO (Maybe Double)
reducerSparkingMachine f input = do
    x <- runT1 machine
    return (fmap f x)
  where
    machine :: MachineT IO k m
    machine
        =  fold mappend mempty
        <~ sparking
        <~ mapping (foldl' snoc mempty)
        <~ buffered 10000
        <~ source input

tdigestDirect
    :: forall comp. KnownNat comp
    => Maybe FilePath -> SomeContDistr -> Proxy comp -> [Double] -> IO (Maybe Double)
tdigestDirect fp d _ input = do
    let mdigest = Just $ TDT.validate $ foldl' (flip TDT.insert) mempty input
    case mdigest of
        Nothing             -> return Nothing
        Just (Left err)     -> fail $ "Validation error: " ++ err
        Just (Right digest) -> do
            printStats treeLike fp d (digest :: TDT.TDigest comp)
            return $ median digest

tdigestVecDirect
    :: forall comp. KnownNat comp
    => Maybe FilePath -> SomeContDistr -> Proxy comp -> [Double] -> IO (Maybe Double)
tdigestVecDirect fp d _ input = do
    let mdigest = Just $ TDV.validate $ foldl' (flip TDV.insert) mempty input
    case mdigest of
        Nothing             -> return Nothing
        Just (Left err)     -> fail $ "Validation error: " ++ err
        Just (Right digest) -> do
            printStats vectorLike fp d (digest :: TDV.TDigest comp)
            return $ TDV.median digest

tdigestMachine
    :: forall comp. KnownNat comp
    => Maybe FilePath -> SomeContDistr -> Proxy comp -> [Double] -> IO (Maybe Double)
tdigestMachine fp d _ input = do
    mdigest <- fmap TDT.validate <$> runT1 machine
    case mdigest of
        Nothing             -> return Nothing
        Just (Left err)     -> fail $ "Validation error: " ++ err
        Just (Right digest) -> do
            printStats treeLike fp d digest
            return $ median digest
  where
    machine :: MachineT IO k (TDT.TDigest comp)
    machine
        =  fold (flip TDT.insert) mempty
        <~ source input

tdigestBufferedMachine
    :: forall comp. KnownNat comp
    => Maybe FilePath -> SomeContDistr -> Proxy comp -> [Double] -> IO (Maybe Double)
tdigestBufferedMachine fp d _ input = do
    mdigest <- fmap TDT.validate <$> runT1 machine
    case mdigest of
        Nothing             -> return Nothing
        Just (Left err)     -> fail $ "Validation error: " ++ err
        Just (Right digest) -> do
            printStats treeLike fp d digest
            return $ median digest
  where
    machine :: MachineT IO k (TDT.TDigest comp)
    machine
        =  fold mappend mempty
        <~ mapping TDT.tdigest
        <~ buffered 10000
        <~ source input

-- Sparking machine doesn't count
tdigestSparkingMachine
    :: forall comp. KnownNat comp
    => Maybe FilePath -> SomeContDistr -> Proxy comp -> [Double] -> IO (Maybe Double)
tdigestSparkingMachine fp d _ input = do
    mdigest <- fmap TDT.validate <$> runT1 machine
    case mdigest of
        Nothing             -> return Nothing
        Just (Left err)     -> fail $ "Validation error: " ++ err
        Just (Right digest) -> do
            printStats treeLike fp d digest
            return $ median digest
  where
    machine :: MachineT IO k (TDT.TDigest comp)
    machine
        =  fold mappend mempty
        <~ sparking
        <~ mapping TDT.tdigest
        <~ buffered 10000
        <~ source input

data TDigestLike a = TDigestLike
    { maximumValue :: a -> Double
    , minimumValue :: a -> Double
    , size         :: a -> Int
    }

treeLike :: TDigestLike (TDT.TDigest comp)
treeLike = TDigestLike
    { maximumValue = TDT.maximumValue
    , minimumValue = TDT.minimumValue
    , size         = TDT.size
    }

vectorLike :: KnownNat comp => TDigestLike (TDV.TDigest comp)
vectorLike = TDigestLike
    { maximumValue = TDV.maximumValue
    , minimumValue = TDV.minimumValue
    , size         = TDV.size
    }

printStats :: HasHistogram a f => TDigestLike a -> Maybe FilePath -> SomeContDistr -> a -> IO ()
printStats tdl mfp (SomeContDistr d) digest = do
    let showFFloat' = showFFloat (Just 6)

    putStrLn $ "size: " ++ show (size tdl digest)

    -- Extra: print quantiles
    putStrLn "average"
    id $ do
        let tdigestA = fromAffine (-1) $ PP.mean digest :: Double
        let analyticA = Statistics.Distribution.mean d :: Double
        putStrLn
            $ showFFloat' tdigestA
            . showString " "
            . showFFloat' analyticA
            . showString " "
            . showFFloat' (abs $ tdigestA - analyticA)
            $ ""
    putStrLn "stddev"
    id $ do
        let tdigestA = fromAffine (-1) $ PP.stddev digest
        let analyticA = Statistics.Distribution.stdDev d
        putStrLn
            $ showFFloat' tdigestA
            . showString " "
            . showFFloat' analyticA
            . showString " "
            . showFFloat' (abs $ tdigestA - analyticA)
            $ ""
    putStrLn "quantiles"
    for_ ([0.1,0.2..0.9] ++ [0.95,0.99,0.999,0.9999,0.99999]) $ \q -> do
        let tdigestQ    = fromAffine (-1) $ PP.quantile q digest
        let analyticQ   = Statistics.Distribution.quantile d q
        putStrLn
            $ showFFloat' q
            . showString ": "
            . showFFloat' tdigestQ
            . showString " "
            . showFFloat' analyticQ
            . showString " "
            . showFFloat' (abs $ analyticQ - tdigestQ)
            $ ""
    putStrLn "cdf"
    for_ ([0, 0.25, 0.5, 1, 2]) $ \x -> do
        let tdigestC = cdf x digest
        let analyticC = cumulative d x
        putStrLn
            $ showFFloat' x
            . showString ": "
            . showFFloat' tdigestC
            . showString " "
            . showFFloat' analyticC
            . showString " "
            . showFFloat' (abs $ analyticC - tdigestC)
            $ ""
    let mi = minimumValue tdl digest
    let ma = maximumValue tdl digest
{-
 - TODO
    case validateHistogram <$> histogram digest of
        Nothing -> pure ()
        Just (Right _hist) -> do
            {-
            putStrLn $ "Histogram ok"
            _ <- traverse print hist
            -}
            pure ()
        Just (Left err) -> putStrLn $ "Errorneous histogram: " ++ err
-}
    {-
    putStrLn "Debug output"
    debugPrint digest
    -}
    let points = flip map [0,0.01..1] $ \x -> mi + (ma - mi) * x
    for_ mfp $ \fp -> do
        putStrLn $ "Writing to " ++ fp
        Chart.toFile Chart.def fp $ do
            Chart.layout_title Chart..= "Histogram"
            Chart.tdigestPlot' "tdigest" digest
            Chart.plot $ Chart.line "theoretical" [map (\x -> (x, density d x)) points]
            -- Chart.plot $ Chart.line "bin sizes" [tdigestBinSize digest]

tdigestBinSize :: forall comp. KnownNat comp => TDT.TDigest comp -> [(Double, Double)]
tdigestBinSize digest = flip map hist $ \(HistBin mi ma  x w cum) ->
    let d = ma - mi

        q = (w / 2 + cum) / tw
        thr = threshold tw q

        y = thr / d / tw
    in (x, y)
  where
    hist = foldMap toList (histogram digest)
    tw = totalWeight digest

    compression :: Double
    compression = fromInteger $ natVal (Proxy :: Proxy comp)

    threshold n q = 4 * n * q * (1 - q) / compression

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

-------------------------------------------------------------------------------
-- Average
-------------------------------------------------------------------------------

data Average a = Average { _samples :: !a, getAverage :: !a }
  deriving (Eq, Show)

instance (Eq a, Fractional a) => Semigroup (Average a) where
    a@(Average n x) <> b@(Average n' x')
        | n == 0    = b
        | n' == 0   = a
        | otherwise = Average m y
      where
        m = n + n'
        y = (n * x + n' * x') / m

instance (Eq a, Fractional a) => Monoid (Average a) where
    mempty = Average 0 0
    mappend = (<>)

instance (Eq a, Fractional a) => Reducer a (Average a) where
    unit = Average 1
