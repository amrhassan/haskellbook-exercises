module Ch18 where

import Control.Monad (join, liftM2)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Monoid
import Data.Traversable


bind :: Monad m => (a -> m b) -> m a -> m b
bind f fa = join (fmap f fa)

-----------------------------------------

data Nope a = NopeDotJpg
  deriving (Eq, Show)

instance Functor Nope where
  fmap = const . const NopeDotJpg

instance Applicative Nope where
  pure = const NopeDotJpg
  ff <*> fa = NopeDotJpg

instance Monad Nope where
  (>>=) = const . const NopeDotJpg

instance Eq a => EqProp (Nope a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Nope a) where
  arbitrary = pure NopeDotJpg

-----------------------------------------

data PhhhbbtttEither b a = LLeft a | RRight b
  deriving (Show, Eq)

instance Functor (PhhhbbtttEither b) where
  f `fmap` (LLeft x) = LLeft (f x)
  _ `fmap` (RRight x) = RRight x

instance Applicative (PhhhbbtttEither b) where
  (LLeft f) <*> (LLeft x) = LLeft (f x)
  _ <*> (RRight x) = RRight x
  (RRight x) <*> _ = RRight x
  pure = LLeft

instance Monad (PhhhbbtttEither b) where
  (LLeft x) >>= f = f x
  (RRight x) >>= _ = RRight x

instance (Arbitrary b, Arbitrary a) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    elements [LLeft x, RRight y]

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where
  (=-=) = eq


-----------------------------------------

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  f `fmap` (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity x) = Identity (f x)

instance Monad Identity where
  (Identity x) >>= f = f x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

-----------------------------------------

data List a = Nil | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where

  _ `fmap` Nil = Nil
  f `fmap` (Cons x tail) = Cons (f x) (fmap f tail)

instance Monoid (List a) where

  mempty = Nil

  Nil `mappend` xs = xs
  xs `mappend` Nil = xs
  (Cons x xs) `mappend` ys = Cons x (xs <> ys)

instance Applicative List where

  pure x = Cons x Nil

  (Cons f fs) <*> allxs@(Cons x xs) = Cons (f x) (f `fmap` xs) <> (fs <*> allxs)
  _ <*> _ = Nil

instance Monad List where
  Nil >>= f = Nil
  (Cons x xs) >>= f = f x <> (xs >>= f)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    x <- arbitrary
    xs <- arbitrary
    elements [Nil, Cons x xs]

instance Eq a => EqProp (List a) where
  (=-=) = eq

fold :: (a -> b -> b) -> b -> List a -> b
fold _ z Nil = z
fold f z (Cons x xs) = f x (fold f z xs)

concat' :: List (List a) -> List a
concat' = fold mappend mempty


-----------------------------------------

j :: Monad m => m (m a) -> m a
j = (>>= id)

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2

a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh = for


main = do
--   let trigger = undefined :: List (Int, String, Char)
--   quickBatch $ monad trigger
  print $ j [[1, 2], [], [3]]
