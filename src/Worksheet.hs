module Worksheet where

import Data.List (intersperse, intercalate)

data DividedResult = Result Integer | DividedByZero
  deriving Show

dividedBy :: Integer -> Integer -> DividedResult
dividedBy _ 0 = DividedByZero
dividedBy x 1 = Result x
dividedBy x y
  | abs x < abs y = Result 0
  | otherwise = case dividedBy (abs x - abs y) (abs y) of
      Result n -> result (n + 1)
      DividedByZero -> DividedByZero
  where result r = if signum x == signum y then Result r else Result (-1 * r)


sumUpTo :: (Eq a, Num a) => a -> a
sumUpTo 1 = 1
sumUpTo n = n + sumUpTo (n - 1)

mult :: (Integral a) => a -> a -> a
mult x 1 = x
mult x y = x + mult x (y - 1)


mc91 :: (Num a, Ord a) => a -> a
mc91 n
  | n > 100 = n - 10
  | otherwise = (mc91 . mc91) (n + 11)


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


digits :: Int -> [Int]
digits n
  | n < 10 = [n]
  | otherwise = digits rest ++ [rightmost]
  where (rest, rightmost) = n `divMod` 10

wordNumber :: Int -> String
wordNumber = intercalate "-" . fmap digitToWord . digits

main = do
  print $ wordNumber 123456
