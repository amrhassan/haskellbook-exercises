module Chapter9 where

import Data.Char

eftBool :: Bool -> Bool -> [Bool]
eftBool x y
  | x > y = []                          -- Already past end
  | x = [x]                             -- Last element
  | x == y = [x]                        -- From-to same element
  | otherwise = x : eftBool (succ x) y  -- All others


eftOrdering :: Ordering -> Ordering -> [Ordering]
eftOrdering x y
  | x > y = []
  | x == GT = [x]
  | x == y = [x]
  | otherwise = x : eftOrdering (succ x) y

myWords :: String -> [String]
myWords [] = []
myWords sentence = firstWord : myWords rest
  where firstWord = takeWhile (/= ' ') sentence
        rest = (dropWhile (== ' ') . dropWhile (/=' ')) sentence

splitList :: (Eq a) => a -> [a] -> [[a]]
splitList _ [] = []
splitList sep list = first : splitList sep rest
  where first = takeWhile (/= sep) list
        rest = (dropWhile (== sep) . dropWhile (/= sep)) list


onlyUpper :: String -> String
onlyUpper = filter isUpper

-- Capitalizes first letter of string
titleCase :: String -> String
titleCase [] = []
titleCase (x:xs) = toUpper x : xs

type CaesarKey = Int

caesarEnc :: CaesarKey -> String -> String
caesarEnc key = fmap enc
  where enc c
          | isAlpha c = chr (((ord c - letterOffset c + key) `mod` letterCount) + letterOffset c)
          | otherwise = c
        letterOffset c = if isUpper c then ord 'A' else ord 'a'
        letterCount = 26

caesarDec :: CaesarKey -> String -> String
caesarDec key = caesarEnc (- key)

myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny p = foldr (\x z -> p x || z) False

myElem :: (Eq a) => a -> [a] -> Bool
myElem x = myAny (== x)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = error "Must give non-empty list"
myMaximumBy cmp list = foldl1 (\x y -> if (x `cmp` y) == GT then x else y) list
-- myMaximumBy _ [x] = x
-- myMaximumBy cmp (x:y:xs) = myMaximumBy cmp ((if (x `cmp` y) == GT then x else y) : xs)

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = error "Must give non-empty list"
myMinimumBy cmp list = foldl1 (\x y -> if (x `cmp` y) == LT then x else y) list

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare

mfldr :: (a -> b -> b) -> b -> [a] -> b
mfldr _ z [] = z
mfldr f z (x:xs) = f x (mfldr f z xs)

main = print $ myMaximumBy (\_ _ -> GT) [1..10]
