module Ch21 where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Identity a = Identity a
  deriving (Show, Eq)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Foldable Identity where
  foldr f z (Identity x) = f x z

instance Traversable Identity where
  traverse f (Identity x) = Identity <$> f x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

-------------

newtype Constant a b = Constant { getConstant :: a }
  deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Foldable (Constant a) where
  foldr _ z (Constant x) = z

instance Traversable (Constant a) where
  traverse _ (Constant x) = Constant <$> pure x

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq

instance (Arbitrary a) => Arbitrary (Constant a b) where
  arbitrary = do
    x <- arbitrary
    return (Constant x)

---------------

data Optional a = Nada | Yep a
  deriving (Show, Eq)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep x) = Yep (f x)

instance Foldable Optional where
  foldr _ z Nada = z
  foldr f z (Yep x) = f x z

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep x) = Yep <$> f x

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = do
    x <- arbitrary
    elements [Nada, Yep x]

------------------

data List a = Nil | Cons a (List a)
  deriving (Show, Eq)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x tail) = Cons (f x) (f <$> tail)

instance Foldable List where
  foldr f z Nil = z
  foldr f z (Cons x tail) = f x (foldr f z tail)

instance Traversable List where
  traverse f Nil = pure Nil
  traverse f (Cons x tail) = Cons <$> f x <*> traverse f tail

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    x <- arbitrary
    xs <- arbitrary
    elements [Nil, Cons x xs]

-------------------------------------------------------

data Three a b c = Three a b c
  deriving (Show, Eq)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance Foldable (Three a b) where
  foldr f z (Three i j k) = f k z

instance Traversable (Three a b) where
  traverse f (Three x y z) = Three x y <$> f z

----------------------------------------------------------

data Three' a b = Three' a b b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' i j k) = Three' i (f j) (f k)

instance Foldable (Three' a) where
  foldr f z (Three' i j k) = j `f` (k `f` z)

instance Traversable (Three' a) where
  traverse f (Three' i j k) = Three' i <$> f j <*> f k

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    pure $ Three' x y z

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq


------------------------

data S n a = S (n a) a      -- NonEmptyList ?
  deriving (Show, Eq)

instance Functor n => Functor (S n) where
  fmap f (S na a) = S (f <$> na) (f $ a)

instance Foldable n => Foldable (S n) where
  foldr f z (S na a) = a `f` (foldr f z na)

instance Traversable n => Traversable (S n) where
--   traverse f (S na a) = (flip S) <$> (f a) <*> (traverse f na)  -- Didn't work with original order of S
  traverse f (S na a) = S <$> (traverse f na) <*> (f a)

instance (Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
  arbitrary = do
    na <- arbitrary
    a <- arbitrary
    return $ S na a

instance (Eq a, Eq (n a)) => EqProp (S n a) where
  (=-=) = eq

------------------------

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node left x right) = Node (f <$> left) (f x) (f <$> right)

instance Foldable Tree where
  foldr _ z Empty = z
  foldr f z (Leaf x) = f x z
  foldr f z (Node left x right) = foldr f (x `f` (foldr f z right)) left

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf x) = Leaf <$> (f x)
  traverse f (Node left x right) = Node <$> (traverse f left) <*> (f x) <*> (traverse f right)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = do
    x <- arbitrary
    left <- arbitrary
    right <- arbitrary
    elements [Empty, Leaf x, Node left x right]

instance Eq a => EqProp (Tree a) where
  (=-=) = eq

main = do
--   let trigger = undefined :: Three' (Int, Bool, [Int]) (Int, Bool, [Int])
  let trigger = undefined :: S List (Int, Bool, [Int])
  quickBatch (functor trigger)
  quickBatch (traversable trigger)
