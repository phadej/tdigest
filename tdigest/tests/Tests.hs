{-# LANGUAGE DataKinds #-}
module Main (main) where

import Data.TDigest
import Test.Tasty
import Test.Tasty.QuickCheck

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

propHistogramIsValid :: [Double] -> Property
propHistogramIsValid ds = case fmap validateHistogram $ histogram td of
    Nothing         -> property True
    Just (Right _)  -> property True
    Just (Left err) -> counterexample msg $ property False
      where
        msg = "Error: "   ++ err     ++ ", " ++
              "TDigest: " ++ show td
  where
    td = tdigest ds :: TDigest 2
