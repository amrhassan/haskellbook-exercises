{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Chapter11 where

import Data.Char
import Data.List

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany = (> 42)

newtype Goats = Goats Int
  deriving (Show, Eq, TooMany)

instance TooMany (Int, String) where
  tooMany (x, _) = tooMany x


data Fruit = Peach | Plum | Apple | Blackberry
  deriving (Eq, Show, Ord)

data JamJars = Jam {fruit :: Fruit, count :: Int}
  deriving (Eq, Show)

totalCount :: [JamJars] -> Int
totalCount = sum . fmap count

mostJars :: [JamJars] -> JamJars
mostJars = last . sortOn count

data BinaryTree a =
  Leaf | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)


treeInsert :: (Ord a) => BinaryTree a -> a -> BinaryTree a
treeInsert Leaf x = Node Leaf x Leaf
treeInsert node@(Node left i right) x
  | x < i = Node (treeInsert left x) i right
  | x > i = Node left i (treeInsert right x)
  | otherwise = node

treeMap :: BinaryTree a -> (a -> b) -> BinaryTree b
treeMap Leaf _ = Leaf
treeMap (Node left x right) f = Node (treeMap left f) (f x) (treeMap right f)


-- In-order serialization of a BinaryTree
treeToList :: BinaryTree a -> [a]
treeToList Leaf = []
treeToList (Node left x right) = treeToList left ++ [x] ++ treeToList right

-- ¯\_(ツ)_/¯
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ z Leaf = z
foldTree f z (Node left x right) = f x lf
  where lf = foldTree f rf left
        rf = foldTree f z right


subsub :: (Eq a) => [a] -> [a] -> Bool
subsub [] _ = True
subsub first@(x:xs) (y:ys)
  | x == y = subsub xs ys
  | otherwise = subsub first ys

-- capitals :: String -> [(String, String)]
-- capitals = fmap (\str@(x:xs) -> )
--   where f [] = ([], [])
--         f str@(x:xs) = (str, )

main = print $ tooMany (303::Int, "Amr")
