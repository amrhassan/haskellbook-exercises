{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}

module Ch16 where

import Test.QuickCheck
import Test.QuickCheck.Function

-----------------

-- Functor laws

functorId :: (Functor f, Eq (f a)) => f a -> Bool
functorId fa = (id <$> fa) == (id $ fa)

functorComp :: (Functor f, Eq (f c)) => f a -> (Fun b c) -> (Fun a b) -> Bool
functorComp fa (Fun _ f) (Fun _ g) = ((f . g) <$> fa) == (f <$> (g <$> fa))

-----------------

newtype Identity a = Identity a
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

-----------------

data Pair a = Pair a a
  deriving (Show, Eq)

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (Pair x y)

instance Functor Pair where
  f `fmap` (Pair x y) = Pair (f x) (f y)

-----------------

data Two a b = Two a b
  deriving (Show, Eq)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (Two x y)

instance Functor (Two a) where
  f `fmap` (Two x y) = Two x (f y)

-----------------

data Trivial = Trivial
  deriving (Eq, Show)

instance Arbitrary Trivial where
  arbitrary = return Trivial

-- No because it's of kind * and functors must be of kind * -> *
-- instance Functor Arbitrary where
--

-----------------

type ANat f g = forall a. f a -> g a

-----------------

newtype Flip f a b = Flip (f b a)
  deriving (Eq, Show)

newtype K a b = K a
  deriving (Show, Eq)

instance Functor (Flip K a) where
  fmap f (Flip (K a)) = Flip (K (f a))

instance (Arbitrary a, Arbitrary b) => Arbitrary (Flip K a b) where
  arbitrary = do
    a <- arbitrary
    return (Flip (K a))

-----------------

data LiftItOut (f :: * -> *) a = LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
  f `fmap` (LiftItOut fa) =  LiftItOut (fmap f fa)


main = do
  quickCheck (functorId :: Flip K String Int -> Bool)
  quickCheck (functorComp :: Flip K String Int -> (Fun Char String) -> (Fun Int Char) -> Bool)
  return ()
