module Ch9WB where

import Data.Char
import Control.Lens

chq2 :: String -> String
chq2 = filter isUpper

titleCase :: String -> String
-- titleCase [] = []
-- titleCase (x:xs) = toUpper x : xs
titleCase = over _head toUpper

toUpperString :: String -> String
toUpperString [] = []
toUpperString (x:xs) = toUpper x : toUpperString xs

firstCap :: String -> Char
firstCap = toUpper . head

type CaesarKey = Int

caesarEnc :: CaesarKey -> String -> String
caesarEnc k = fmap encChr
  where encChr c
          | isUpper c = move (ord 'A') c
          | isLower c = move (ord 'a') c
          | otherwise = c
        move offset c = chr $ ((ord c - offset) + k) `mod` 26 + offset

caesarDec :: CaesarKey -> String -> String
caesarDec k = caesarEnc (negate k)

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = x && myAnd xs


myOr :: [Bool] -> Bool
-- myOr [] = False
-- myOr (x:xs) = x || myOr xs
myOr bs = not $ myAnd (fmap not bs)


myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs

myElem :: (Eq a) => a -> [a] -> Bool
-- myElem _ [] = False
-- myElem y (x:xs) = y == x || myElem y xs
myElem x = myAny (== x)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
-- squish [] = []
-- squish (x:xs) = x ++ squish xs
squish = squishMap id

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = error "Empty list"
myMaximumBy _ [x] = x
myMaximumBy cmp (x:xs) =
  let other = myMaximumBy cmp xs in
    if cmp x other == GT then
      x
    else
      other

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy cmp = myMaximumBy (\ x y -> if (cmp x y) == GT then LT else GT)



main = print $ myMinimumBy (\ _ _ -> GT) [1..10]
