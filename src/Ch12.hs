module Ch12 where

import Data.List

replaceThe :: String -> String
replaceThe = unwords . fmap (\x -> if x == "the" then "a" else x) . words

vowels = "aeiouy"

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel [] = 0
countTheBeforeVowel ('t' : 'h' : 'e' : ' ' : v : rest)
  | v `elem` vowels = 1 + countTheBeforeVowel rest
  | otherwise = countTheBeforeVowel rest
countTheBeforeVowel (_:xs) = countTheBeforeVowel xs

vowelCount :: String -> Integer
vowelCount = toInteger . length . filter (`elem` vowels)

consonantCount :: String -> Integer
consonantCount xs = (toInteger . length) xs - vowelCount xs

newtype Word' = Word' String
  deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord str
  | consonantCount str >= vowelCount str = Just (Word' str)
  | otherwise = Nothing

data Nat = Zero | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat n
  | n < 0 = Nothing
  | n == 0 = Just Zero
  | otherwise = fmap Succ (integerToNat (n-1))

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

isNothing :: Maybe a -> Bool
isNothing = not . isJust

maybee :: b -> (a -> b) -> Maybe a -> b
maybee z _ Nothing = z
maybee _ f (Just x) = f x

fromMaybe :: a -> Maybe a -> a
fromMaybe fallback Nothing = fallback
fromMaybe _ (Just x) = x

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes = concatMap maybeToList

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe (Just x : xs) = (x:) <$> flipMaybe xs
flipMaybe (Nothing : _) = Nothing

left :: Either a b -> Maybe a
left (Left x) = Just x
left _ = Nothing

right :: Either a b -> Maybe b
right (Right x) = Just x
right _ = Nothing

lefts' :: [Either a b] -> [a]
lefts' = catMaybes . fmap left

rights' :: [Either a b] -> [b]
rights' = catMaybes . fmap right

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f = fmap f . right

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' fl _ (Left x) = fl x
either' _ fr (Right x) = fr x

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)


---

iterate' :: (a -> a) -> a -> [a]
iterate' inc x = x : iterate' inc (inc x)

unfoldr' :: (s -> Maybe (a, s)) -> s -> [a]
unfoldr' f state = case f state of
  Just (a, newState) -> a : unfoldr' f newState
  Nothing -> []

iterate'' :: (a -> a) -> a -> [a]
iterate'' f = unfoldr' (\s -> Just (s, f s))


data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)
  deriving (Show, Eq, Ord)


-- Anamorphism for a tree
unfoldTree :: (s -> Maybe (s, a, s)) -> s -> BinaryTree a
unfoldTree f state = case f state of
  Nothing -> Leaf
  Just (leftState, x, rightState) -> Node (unfoldTree f leftState) x (unfoldTree f rightState)

treeBuilder :: Integer -> BinaryTree Integer
treeBuilder n = unfoldTree transition 0
  where transition x
                    | x > n = Nothing
                    | otherwise = Just (x+1, x, x+1)


main = print $ treeBuilder 2
