module Ch21W where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Foldable Identity where
  foldr f z (Identity x) = f x z

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Traversable Identity where
  traverse f (Identity x) = Identity <$> f x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq


------------

newtype Constant a b =
  Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Foldable (Constant a) where
  foldr _ z _ = z

instance Functor (Constant a) where
  fmap f (Constant x) = Constant x

instance Traversable (Constant a) where
  traverse f (Constant x) = pure $ Constant x

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

-----------

data Optional a = Nada | Yep a

instance Foldable Optional where
  foldr f z (Yep x) = f x z
  foldr _ z Nada    = z

instance Functor Optional where
  fmap f (Yep x) = Yep $ f x
  fmap _ Nada    = Nada

instance Traversable Optional where
  traverse f Nada    = pure Nada
  traverse f (Yep x) = Yep <$> f x

----------

data List a = Nil | Cons a (List a)

instance Foldable List where
  foldr f z Nil = z
  foldr f z (Cons x xs) = f x $ foldr f z xs

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Traversable List where
  traverse _ Nil          = pure Nil
  traverse f (Cons x xs)  = Cons <$> f x <*> traverse f xs


------------------

------------------

main = do
--   let trigger = undefined :: Three' (Int, Bool, [Int]) (Int, Bool, [Int])
  let trigger = undefined :: Identity (Int, Bool, [Int])
  quickBatch (functor trigger)
  quickBatch (traversable trigger)
