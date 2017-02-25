{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main, tdigestBinSize) where

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
import GHC.TypeLits                (KnownNat, SomeNat (..), natVal, someNatVal)
import Statistics.Distribution     (ContGen (..), density)

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
import           Graphics.Rendering.Chart.Easy             ((&), (.~), (^.))
import qualified Graphics.Rendering.Chart.Easy             as Chart

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
    | DistribStandard
  deriving (Show)

timed :: Show a => IO a -> IO ()
timed mx = do
    s <- getCurrentTime
    x <- mx
    print x
    e <- getCurrentTime
    print (diffUTCTime e s)

action :: Method -> Distrib -> Int -> Int -> Word32 -> Maybe FilePath -> IO ()
action m d s c iseed fp = do
    print (m, d, s, c)
    let seed = initSeed (V.singleton iseed)
    let dens = case d of
            DistribIncr     -> density $ uniformDistr 0 (fromIntegral s)
            DistribUniform  -> density $ uniformDistr 0 1     -- median around 0.5
            DistribExponent -> density $ exponential $ log 2  -- median around 1.0
            DistribGamma    -> density $ gammaDistr 0.1 0.1   -- median around .0000593391
            DistribStandard -> density standard
    let input = take s $ case d of
            DistribIncr     -> [1 .. fromIntegral s] -- not sure, but end point prevents floating
            DistribUniform  -> randomStream (uniformDistr 0 1) seed     -- median around 0.5
            DistribExponent -> randomStream (exponential $ log 2) seed  -- median around 1.0
            DistribGamma    -> randomStream (gammaDistr 0.1 0.1) seed   -- median around .0000593391
            DistribStandard -> randomStream standard seed
    let method = case m of
          MethodAverage         -> pure . average
          MethodNaive           -> pure . naiveMedian
          MethodVector          -> pure . vectorMedian
          MethodTDigest         -> reifyNat c $ tdigestMachine fp dens
          MethodTDigestBuffered -> reifyNat c $ tdigestBufferedMachine fp dens
          MethodTDigestSparking -> reifyNat c $ tdigestSparkingMachine fp dens
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
    readMethod "average"  = Just MethodAverage
    readMethod "naive"    = Just MethodNaive
    readMethod "vector"   = Just MethodVector
    readMethod "digest"   = Just MethodTDigest
    readMethod "tdigest"  = Just MethodTDigest
    readMethod "buffered" = Just MethodTDigestBuffered
    readMethod "sparking" = Just MethodTDigestSparking
    readMethod _          = Nothing

    readDistrib "incr"     = Just DistribIncr
    readDistrib "uniform"  = Just DistribUniform
    readDistrib "exponent" = Just DistribExponent
    readDistrib "standard" = Just DistribStandard
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

tdigestMachine
    :: forall comp. KnownNat comp
    => Maybe FilePath -> (Double -> Double) -> Proxy comp -> [Double] -> IO (Maybe Double)
tdigestMachine fp dens _ input = do
    mdigest <- fmap validate <$> runT1 machine
    case mdigest of
        Nothing             -> return Nothing
        Just (Left err)     -> fail $ "Validation error: " ++ err
        Just (Right digest) -> do
            printStats fp dens digest
            return $ median digest
  where
    machine :: MachineT IO k (TDigest comp)
    machine
        =  fold (flip insert) mempty
        <~ source input

tdigestBufferedMachine
    :: forall comp. KnownNat comp
    => Maybe FilePath -> (Double -> Double) -> Proxy comp -> [Double] -> IO (Maybe Double)
tdigestBufferedMachine fp dens _ input = do
    mdigest <- fmap validate <$> runT1 machine
    case mdigest of
        Nothing             -> return Nothing
        Just (Left err)     -> fail $ "Validation error: " ++ err
        Just (Right digest) -> do
            printStats fp dens digest
            return $ median digest
  where
    machine :: MachineT IO k (TDigest comp)
    machine
        =  fold mappend mempty
        <~ mapping tdigest
        <~ buffered 10000
        <~ source input

-- Sparking machine doesn't count
tdigestSparkingMachine
    :: forall comp. KnownNat comp
    => Maybe FilePath -> (Double -> Double) -> Proxy comp -> [Double] -> IO (Maybe Double)
tdigestSparkingMachine fp dens _ input = do
    mdigest <- fmap validate <$> runT1 machine
    case mdigest of
        Nothing             -> return Nothing
        Just (Left err)     -> fail $ "Validation error: " ++ err
        Just (Right digest) -> do
            printStats fp dens digest
            return $ median digest
  where
    machine :: MachineT IO k (TDigest comp)
    machine
        =  fold mappend mempty
        <~ sparking
        <~ mapping tdigest
        <~ buffered 10000
        <~ source input

printStats :: KnownNat comp => Maybe FilePath -> (Double -> Double) -> TDigest comp -> IO ()
printStats mfp dens digest = do
    -- Extra: print quantiles
    putStrLn "quantiles"
    for_ ([0.1,0.2..0.9] ++ [0.95,0.99,0.999,0.9999,0.99999]) $ \q ->
        putStrLn $ show q ++ ":" ++ show (quantile q digest)
    putStrLn "cdf"
    for_ ([0, 0.25, 0.5, 1, 2]) $ \x ->
        putStrLn $ show x ++ ": " ++ show (cdf x digest)
    let mi = minimumValue digest
    let ma = maximumValue digest
    case validateHistogram (histogram digest) of
        Right _hist -> do
            {-
            putStrLn $ "Histogram ok"
            _ <- traverse print hist
            -}
            pure ()
        Left err -> putStrLn $ "Errorneous histogram: " ++ err
    {-
    putStrLn "Debug output"
    debugPrint digest
    -}
    let points = flip map [0,0.01..1] $ \x -> mi + (ma - mi) * x
    for_ mfp $ \fp -> do
        putStrLn $ "Writing to " ++ fp
        Chart.toFile Chart.def fp $ do
            Chart.layout_title Chart..= "Histogram"
            color <- Chart.takeColor
            let lineStyle = Chart.def & Chart.line_color .~ color
            Chart.plot $ pure $ tdigestToPlot lineStyle digest
            Chart.plot $ Chart.line "theoretical" [map (\x -> (x, dens x)) points]
            -- Chart.plot $ Chart.line "bin sizes" [tdigestBinSize digest]

tdigestBinSize :: forall comp. KnownNat comp => TDigest comp -> [(Double, Double)]
tdigestBinSize digest = flip map hist $ \(HistBin mi ma w cum) ->
    let x = (ma + mi) / 2
        d = ma - mi

        q = (w / 2 + cum) / tw
        thr = threshold tw q

        y = thr / d / tw
    in (x, y)
  where
    hist = histogram digest
    tw = totalWeight digest

    compression :: Double
    compression = fromInteger $ natVal (Proxy :: Proxy comp)

    threshold n q = 4 * n * q * (1 - q) / compression

tdigestToPlot :: Chart.LineStyle -> TDigest comp -> Chart.Plot Double Double
tdigestToPlot lineStyle digest = Chart.Plot
    { Chart._plot_render     = renderHistogram
    , Chart._plot_legend     = []
    , Chart._plot_all_points = unzip allPoints
    }
  where
    hist = histogram digest
    allPoints = flip map hist $ \(HistBin mi ma w _) ->
        let x = (ma + mi) / 2
            d = ma - mi
            y = w / d / tw
        in (x, y)
    tw = totalWeight digest

    renderHistogram pmap = do
        let fillColor = Chart.blend 0.5 (Chart.opaque Chart.white) (lineStyle ^. Chart.line_color)
        let fillStyle = Chart.def & Chart.fill_color .~ fillColor
        Chart.withLineStyle lineStyle $ Chart.withFillStyle fillStyle $
            for_ hist $ \(HistBin mi ma w _) -> do
                let d = ma - mi
                    y = w / d / tw
                    path = Chart.rectPath $ Chart.Rect
                        (Chart.mapXY pmap (mi,0))
                        (Chart.mapXY pmap (ma,y))
                Chart.alignFillPath path >>= Chart.fillPath
                Chart.alignStrokePath path >>= Chart.strokePath

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
