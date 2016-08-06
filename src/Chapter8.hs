-- {-# LANGUAGE FlexibleInstances #-}

module Chapter8 where

import Data.List (intersperse)

fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

type Numerator = Integer
type Denominator = Integer
type Quotient = Integer
type Remainder = Integer

data DividedResult = Result (Quotient, Remainder) | DividedByZero
  deriving (Show)

-- Outcome satisfies the property:
--    If denom /= 0:
--        numerator = quotient * denominator + remainder
dividedBy :: Numerator -> Denominator -> DividedResult
dividedBy num denom
  | denom == 0 = DividedByZero
  | numSign /= denomSign = negative $ abs num `dividedBy` abs denom
  | otherwise = Result (iter (abs num) (abs denom) 0)
  where iter n d count
          | n < d = (count, n)
          | otherwise = iter (n - d) d (count + 1)
        numSign = sign num
        denomSign = sign denom
        negative (Result (q, r)) = Result (-q, ofSign numSign r)


data Sign = Negative | Zero | Positive
  deriving (Eq, Show)

-- Determines the sign of the given numeral
sign :: (Ord a, Num a) => a -> Sign
sign x
  | x < 0 = Negative
  | x == 0 = Zero
  | x > 0 = Positive

-- Puts the given numeral into the given sign
ofSign :: (Num a) => Sign -> a -> a
ofSign Negative x = - (abs x)
ofSign Zero _ = 0
ofSign Positive x = abs x

sumUpTo :: (Num a, Enum a) => a -> a
sumUpTo n = sum $ enumFrom n

times :: (Eq a, Num a) => a -> a -> a
times 1 x = x
times n x = x + ((n - 1) `times` x)

mc91 :: (Ord a, Num a) => a -> a
mc91 n
  | n > 100 = n - 10
  | otherwise = (mc91 . mc91) $ n + 11


digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = error "Not a single digit"

digits :: Int -> [Int]
digits n
  | n < 10 = [n]
  | otherwise = digits rest ++ [last]
    where (rest, last) = n `divMod` 10


wordNumber :: Int -> String
wordNumber = foldl1 (\x y -> x ++ "-" ++ y) . fmap digitToWord . digits


main = print $ wordNumber 12345
