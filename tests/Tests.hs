{-# LANGUAGE DataKinds #-}
module Main (main) where

import Data.TDigest
import Test.Tasty
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "properties"
    [ testProperty "valid" propValid
    ]

propValid :: [Double] -> Property
propValid ds = case validate td of
    Right _  -> property True
    Left err -> counterexample (err ++ " " ++ show td) (valid td)
  where
    td = tdigest ds :: TDigest 2
