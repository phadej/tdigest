{-# LANGUAGE DataKinds #-}
module Main (main) where

import Data.TDigest
import Test.Tasty
import Test.Tasty.QuickCheck
import Control.Monad

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "properties"
    [ testProperty "tdigest validity"   propTDigestIsValid
    , testProperty "histogram validity" propHistogramIsValid
    ]

propTDigestIsValid :: [Double] -> Property
propTDigestIsValid ds = case validate td of
    Right _  -> property True
    Left err -> counterexample (err ++ " " ++ show td) (valid td)
  where
    td = tdigest ds :: TDigest 2

checkHistogramInvariants :: [HistBin] -> Either (String, HistBin, HistBin) ()
checkHistogramInvariants = mapM_ valid . pairs
  where
    valid (lb@(HistBin _ lmax lwt lcw), rb@(HistBin rmin _ _ rcw))
        = do check (lmax == rmin)     "gap between bins"
             check (lcw + lwt == rcw) "mismatch in weight cumulation"
      where
        check cond err = unless cond $ Left (err, lb, rb)
    pairs xs = zip xs $ tail xs

propHistogramIsValid :: [Double] -> Property
propHistogramIsValid ds =
    case checkHistogramInvariants $ histogram td of
      Right ()           -> property True
      Left (err, lb, rb) -> counterexample msg $ property False
        where
          msg = "Error: "   ++ err     ++ ", " ++
                "Left: "    ++ show lb ++ ", " ++
                "Right: "   ++ show rb ++ ", " ++
                "TDigest: " ++ show td
  where
    td = tdigest ds :: TDigest 2
