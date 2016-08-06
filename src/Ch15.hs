module Ch15 where

import Data.Semigroup
import Test.QuickCheck

data Trivial = Trivial
  deriving (Eq, Show)


instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

instance Arbitrary Trivial where
  arbitrary = return Trivial

-----------------------------------------------

newtype Identity a = Identity a
  deriving (Show, Eq)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    x <- arbitrary
    return (Identity x)

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (x <> y)


-----------------------------------------------

data Two a b = Two a b
  deriving (Show, Eq)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two x y) <> (Two j k) = Two (x <> j) (y <> k)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (Two x y)

-----------------------------------------------

data Three a b c = Three a b c
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three x y z) <> (Three i j k) = Three (x <> i) (y <> j) (z <> k)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (Three x y z)

-----------------------------------------------

newtype BoolConj = BoolConj Bool
  deriving (Eq, Show)

instance Semigroup (BoolConj) where
  (BoolConj x) <> (BoolConj y) = BoolConj (x && y)

instance Arbitrary BoolConj where
  arbitrary = do
    b <- arbitrary
    return (BoolConj b)

-----------------------------------------------

data Or a b = Fst a | Snd b
  deriving (Eq, Show)

instance Semigroup (Or a b) where
  (Snd x) <> _ = Snd x
  _ <> (Snd x) = Snd x
  _ <> x = x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    elements [Fst x, Snd y]

-----------------------------------------------

newtype Combine a b = Combine { unCombine :: (a -> b) }

instance Semigroup b => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine (\x -> (f x) <> (g x))

instance (Semigroup b, Arbitrary b, CoArbitrary a) => Arbitrary (Combine a b) where
  arbitrary = do
    f <- arbitrary
    return $ Combine f

-----------------------------------------------

newtype Comp a = Comp { unComp :: a -> a }

instance Semigroup (Comp a) where
  (Comp x) <> (Comp y) = Comp (x . y)

-----------------------------------------------

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftId :: (Eq m, Monoid m) => m -> Bool
monoidLeftId x = (mempty `mappend` x) == x

monoidRightId :: (Eq m, Monoid m) => m -> Bool
monoidRightId x = (x `mappend` mempty) == x

type Tested = Or String [Int]

-----------------------------------------------

newtype Mem s a = Mem { runMem :: s -> (a, s) }

instance Monoid a => Monoid (Mem s a) where

  mempty = Mem $ \s -> (mempty, s)

  (Mem f) `mappend` (Mem g) = Mem $ \s -> let (fv, fs) = (f s) in
    let (gv, gs) = (g fs) in
      (fv `mappend` gv, gs)


f' :: Num s => Mem s String
f' = Mem $ \s -> ("hi", s + 1)

-----------------------------------------------

main = do
  print $ runMem (f' <> mempty) 0
  print $ runMem (mempty <> f') 0
  print $ (runMem mempty 0 :: (String, Int))
  print $ runMem (f' <> mempty) 0 == runMem f' 0
  print $ runMem (mempty <> f') 0 == runMem f' 0
