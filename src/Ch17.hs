module Ch17 where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a = Nil | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where

  _ `fmap` Nil = Nil
  f `fmap` (Cons x tail) = Cons (f x) (fmap f tail)

instance Monoid (List a) where

  mempty = Nil

  Nil `mappend` xs = xs
  xs `mappend` Nil = xs
  (Cons x xs) `mappend` ys = Cons x (xs `mappend` ys)

instance Applicative List where

  pure x = Cons x Nil

  (Cons f fs) <*> allxs@(Cons x xs) =
    Cons (f x) (f `fmap` xs) `mappend` (fs <*> allxs)
  _ <*> _ = Nil

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    x <- arbitrary
    xs <- arbitrary
    elements [Nil, Cons x xs]

instance Eq a => EqProp (List a) where
  (=-=) = eq

main = do
  quickBatch $ monoid (Cons (4 :: Int) Nil)
  quickBatch $ functor (Cons (4 :: Int, False :: Bool, 'c') Nil)
  quickBatch $ applicative (Cons (4 :: Int, False :: Bool, 'c') Nil)
