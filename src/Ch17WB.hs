module Ch17WB where

import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers
import Test.QuickCheck


data List a = Nil | Cons a (List a)
  deriving (Show, Eq)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x list) = Cons (f x) (fmap f list)

instance Applicative List where
  pure x = Cons x Nil
  (Cons f list) <*> xs = (f <$> xs) `append` (list <*> xs)
  _ <*> _ = Nil

append :: List a -> List a -> List a
append (Cons x xs) list = Cons x (xs `append` list)
append Nil list = list

instance Eq a => EqProp (List a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    x <- arbitrary
    xs <- arbitrary
    elements [Nil, Cons x xs]

-----------------------------------------

newtype ZipList' a = ZipList' (List a)
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs in take' 3000 l
          ys' = let (ZipList' l) = ys in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' list) = ZipList' (f <$> list)

instance Applicative ZipList' where

  pure = ZipList' . repeat'

  (ZipList' (Cons f fs)) <*> (ZipList' (Cons x xs)) = ZipList' $ Cons (f x) tails
    where (ZipList' tails) = ZipList' fs <*> ZipList' xs

  _ <*> _ = ZipList' Nil

repeat' :: a -> List a
repeat' x = Cons x (repeat' x)

take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons x xs) = Cons x (take' (n-1) xs)

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary


----------------------------------------------

data Validation err a = Failure err | Success a
  deriving (Eq, Show)

data Error = DividedByZero | StackOverflow | MogglesChewedWires
  deriving (Eq, Show)


data Sum a b = First a | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  f `fmap` (Second x) = Second (f x)
  _ `fmap` (First e) = First e

instance Applicative (Sum a) where
  pure = Second
  (Second f) <*> (Second x) = Second (f x)
  First err <*> _ = First err
  _ <*> First err = First err



main = quickBatch $ applicative (ZipList' $ Cons (42 :: Int, False, "") Nil)
