module Chapter7 where

import Test.HUnit

-- tensDigit :: Integral a => a -> a
-- tensDigit x = d
--   where (xLast, _) = x `divMod` 10
--         (_, d) = xLast `divMod` 10

tensDigit :: Integral a => a -> a
tensDigit = nthDigit 1

hundredthDigit :: Integral a => a -> a
hundredthDigit = nthDigit 2

-- Nth decimal digit (0-indexed from the right)
nthDigit :: Integral a => a -> a -> a
nthDigit i x = (x `div` iExp i 10) `mod` 10


-- Integer exponentiation
iExp :: Integral a => a -> a -> a
iExp 0 x = 1
iExp n x
  | even n = iExp (n `div` 2) (x * x)
  | otherwise = x * iExp (n - 1) x


foldBool :: a -> a -> Bool -> a

-- foldBool x y bool = case bool of
--   True -> x
--   False -> y

foldBool x y bool
  | bool = x
  | not bool = y


g :: (a -> b) -> (a, c) -> (b, c)
g ab (a, c) = (ab a, c)

roundTrip :: (Show a, Read b) => a -> b
roundTrip = read . show
