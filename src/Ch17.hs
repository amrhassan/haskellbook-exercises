module Ch17 where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Monoid
import Control.Applicative


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

  (Cons f fs) <*> allxs@(Cons x xs) =
    Cons (f x) (f `fmap` xs) <> (fs <*> allxs)
  _ <*> _ = Nil

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

-- flatMap in Haskell? This is heresy!
flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' (f <$> as)

take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons x xs) = Cons x (take' (n-1) xs)

--------------------------------------

instance Monoid a => Monoid (ZipList a) where
  mempty = pure mempty
  mappend = liftA2 mappend

instance Arbitrary a => Arbitrary (ZipList a) where
  arbitrary = ZipList <$> arbitrary

instance Eq a => EqProp (ZipList a) where
  (=-=) = eq

--------------------------------------

newtype ZipList' a = ZipList' (List a)
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  (=-=) = eq
--   xs =-= ys = xs' `eq` ys'`
--     where xs' = let (ZipList' l) = xs in take' 3000 l
--           ys' = let (ZipList' l) = ys in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where

  pure = ZipList' . repeated
    where repeated x = Cons x (repeated x)

  (ZipList' xs) <*> (ZipList' ys) = ZipList' (xs `zipped` ys)
    where (Cons a as) `zipped` (Cons b bs) = Cons (a b) (as `zipped` bs)
          _ `zipped` _ = Nil

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary


--------------------------------------



--------------------------------------

main = do

  putStrLn ""
  putStrLn "[ZipList]"
  quickBatch $ monoid (ZipList ["fawzy"])

  putStrLn ""
  putStrLn "[List]"
  quickBatch $ monoid (Cons (4 :: Int) Nil)
  quickBatch $ functor (Cons (4 :: Int, False :: Bool, 'c') Nil)
  quickBatch $ applicative (Cons (4 :: Int, False :: Bool, 'c') Nil)

  putStrLn ""
  putStrLn "[ZipList']"
  quickBatch $ functor (ZipList' (Cons (4 :: Int, False :: Bool, 'c') Nil))
  quickBatch $ applicative $ ZipList' (Cons (4 :: Int, False :: Bool, 'c') Nil)
