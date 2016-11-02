{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Control.Category            (Category)
import Control.Monad               (join, when)
import Control.Monad.ST            (runST)
import Control.Parallel            (par)
import Control.Parallel.Strategies (parBuffer, rseq, using)
import Data.List                   (foldl', sort)
import Data.Machine
import Data.Machine.Runner         (runT1)
import Data.Proxy                  (Proxy (..))
import Data.Semigroup              ((<>))
import Data.Time
import Data.Word                   (Word32)
import GHC.TypeLits                (KnownNat)
import System.Environment          (getArgs)
import System.Random.TF.Init       (mkTFGen)
import System.Random.TF.Instances  (Random (..))

import qualified Data.Vector.Algorithms.Intro as Intro
import qualified Data.Vector.Unboxed          as V
import qualified Data.Vector.Unboxed.Mutable  as VU
import qualified Options.Applicative          as O

import Data.TDigest

data Method
    = MethodNaive
    | MethodVector
    | MethodTDigest
    | MethodTDigestBuffered
  deriving (Show)

data Distrib
    = DistribIncr
    | DistribUniform
  deriving (Show)

timed :: Show a => IO a -> IO ()
timed action = do
    s <- getCurrentTime
    x <- action
    print x
    e <- getCurrentTime
    print (diffUTCTime e s)

action :: Method -> Distrib -> Int -> IO ()
action m d s = do
    print (m, d, s)
    let gen = mkTFGen 42
    let input = take s $ case d of
            DistribIncr    -> [1 .. fromIntegral s] -- not sure, but end point prevents floating
            DistribUniform -> randoms gen
    let method = case m of
          MethodNaive           -> pure . naiveMedian
          MethodVector          -> pure . vectorMedian
          MethodTDigest         -> viaMachine
          MethodTDigestBuffered -> viaBufferedMachine
    timed $ method input

actionParser :: O.Parser (IO ())
actionParser = action
    <$> O.option (O.maybeReader readMethod) (
        O.short 'm' <> O.long "method" <> O.metavar ":method" <> O.value MethodTDigest)
    <*> O.option (O.maybeReader readDistrib) (
        O.short 'd' <> O.long "distrib" <> O.metavar ":distrib" <> O.value DistribUniform)
    <*> O.option O.auto (
        O.short 's' <> O.long "size" <> O.metavar ":size" <> O.value 100000)
  where
    readMethod "naive"    = Just MethodNaive
    readMethod "vector"   = Just MethodVector
    readMethod "digest"   = Just MethodTDigest
    readMethod "buffered" = Just MethodTDigestBuffered
    readMethod _          = Nothing

    readDistrib "incr"    = Just DistribIncr
    readDistrib "uniform" = Just DistribUniform
    readDistrib _         = Nothing

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

sparking :: (Category k, Monad m) => MachineT m (k a) a
sparking = mapping (\x -> x `par` x)

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
-- Almost obsolete
-------------------------------------------------------------------------------

-- good enough
instance Random Double where
    randomR = error "randomR @Double: not implemented"
    random g =
        let (w, g') = random g
        in (fromIntegral (w :: Word32) / fromIntegral (maxBound :: Word32), g')
