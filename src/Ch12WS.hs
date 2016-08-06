
module Ch12WS where

import Data.Maybe


notThe :: String -> Maybe String
notThe "the" = Nothing
notThe x = Just x

vowels = "aeiou"

countVowels :: String -> Integer
countVowels =  fromIntegral . length . filter (`elem` vowels)

newtype Word' =
  Word' String
  deriving (Eq, Show)


mkWord :: String -> Maybe Word'
mkWord xs
  | countVowels xs > ((fromIntegral . length) xs - countVowels xs) = Just (Word' xs)
  | otherwise = Nothing

data Nat = Zero | Succ Nat
  deriving (Eq, Show)


natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x

integerToNat :: Integer -> Maybe Nat
integerToNat x
  | x < 0 = Nothing
  | x == 0 = Just Zero
  | x > 0 = Succ <$> integerToNat (x - 1)


mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing = b
mayybee _ f (Just x) = f x


flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe (Just x : xs) = (x :) <$> flipMaybe xs
flipMaybe (Nothing : _) = Nothing

iterate' :: (a -> a) -> a -> [a]
iterate' f x = x : iterate' f (f x)

unfoldr :: (s -> Maybe (a, s)) -> s -> [a]
unfoldr f z = case f z of
  Nothing -> []
  Just (x, ns) -> x : unfoldr f ns


betterIterate :: (a -> a) -> a -> [a]
betterIterate f = unfoldr (\x -> Just (f x, f x))

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)


unfoldBinaryTree :: (s -> Maybe (s, a, s)) -> s -> BinaryTree a
unfoldBinaryTree f z = case f z of
  Nothing -> Leaf
  Just (ls, x, rs) -> Node (unfoldBinaryTree f ls) x (unfoldBinaryTree f rs)


main = print $ integerToNat 1000000000
