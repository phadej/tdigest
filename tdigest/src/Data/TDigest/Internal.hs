module Data.TDigest.Internal where

-- TODO: make newtypes
type Mean = Double
type Weight = Double
type Centroid = (Mean, Weight)
type Size = Int

-------------------------------------------------------------------------------
-- Assert
-------------------------------------------------------------------------------

{-# INLINE assert #-}
assert :: Bool -> String -> a -> a
assert _ _ = \x -> x
{-
assert False msg _ = error msg
assert True  _   x = x
-}

-------------------------------------------------------------------------------
-- Double helpers
-------------------------------------------------------------------------------

eq :: Double -> Double -> Bool
eq a b = abs (a-b) < 1e-6

negInf :: Double
negInf = negate posInf

posInf :: Double
posInf = 1/0
