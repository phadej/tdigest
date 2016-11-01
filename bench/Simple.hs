{-# LANGUAGE ScopedTypeVariables, DataKinds #-}
module Main (main) where

import Control.Category            (Category)
import Control.Monad               (when)
import Control.Parallel            (par)
import Control.Parallel.Strategies (parBuffer, rseq, using)
import Data.List                   (foldl', sort)
import Data.Machine
import Data.Machine.Runner         (runT1)
import Data.Proxy                  (Proxy (..))
import Data.Time
import Data.Word                   (Word32)
import System.Environment          (getArgs)
import System.Random.TF.Init       (mkTFGen)
import System.Random.TF.Instances  (Random (..))
import GHC.TypeLits   (KnownNat)

import Data.TDigest

timed :: Show a => a -> IO ()
timed x = do
    s <- getCurrentTime
    print x
    e <- getCurrentTime
    print (diffUTCTime e s)

naiveMedian :: [Double] -> Maybe Double
naiveMedian [] = Nothing
naiveMedian xs = Just $ sort xs !! (length xs `div` 2)

main :: IO ()
main = do
    args <- getArgs
    let g = mkTFGen 42
    case args of
        ["-naive"]            -> timed (naiveMedian $ map fromIntegral [1..size])
        ["-tdigest"]          -> timed (medianF    (Proxy :: Proxy 10) $ map fromIntegral [1..size])
        ["-tdigest-par"]      -> timed (parMedianF (Proxy :: Proxy 10) $ map fromIntegral [1..size])
        -- TODO: configurable precision
        ["-naive-rand"]       -> timed (naiveMedian $ take size2 $ randoms g)
        ["-tdigest-rand"]     -> do
            res <- viaMachine $ take size2 $ randoms g
            print res
        ["-tdigest-par-rand"] -> do
            res <- viaParallelMachine $ take size2 $ randoms g
            print res
        _ -> pure ()

viaMachine :: [Double] -> IO (Maybe (Maybe Double))
viaMachine input = fmap (median :: TDigest 50 -> Maybe Double) <$> runT1 machine
  where
    machine
        =  fold (flip insert) mempty
        <~ autoM inputAction
        <~ counting
        <~ source input
    inputAction (x, i) = do
        when (i `mod` 100000 == 0) $ putStrLn $ "consumed " ++ show i
        return x

viaParallelMachine :: [Double] -> IO (Maybe (Maybe Double))
viaParallelMachine input = fmap median <$> runT1 machine
  where
    machine
        = fold mappend mempty
        <~ sparking
        <~ mapping (tdigest :: [Double] -> TDigest 50)
        <~ buffered 10000
        <~ autoM inputAction
        <~ counting
        <~ source input
    inputAction (x, i) = do
        when (i `mod` 100000 == 0) $ putStrLn $ "consumed " ++ show i
        return x

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

size :: Int
size = 5000000 

size2 :: Int
size2 = 15000000

medianF
    :: forall comp f. (Foldable f, KnownNat comp)
    => Proxy comp -> f Double -> Maybe Double
medianF _ x = median (tdigest x :: TDigest comp)

parMedianF
    :: forall comp. KnownNat comp
    => Proxy comp -> [Double] -> Maybe Double
parMedianF _
    = median
    . foldl' mappend mempty
    . (\dss -> map (tdigest :: [Double] -> TDigest comp) dss `using` parBuffer 2 rseq)
    . chunkList 10000

-- | Split a list into chunks of /n/ elements.
chunkList :: Int -> [a] -> [[a]]
chunkList _ [] = []
chunkList n xs = as : chunkList n bs where (as,bs) = splitAt n xs

-- good enough
instance Random Double where
    randomR = error "randomR @Double: not implemented"
    random g =
        let (w, g') = random g
        in (fromIntegral (w :: Word32) / fromIntegral (maxBound :: Word32), g')
