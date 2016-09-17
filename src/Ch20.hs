module Ch20 where

import Data.Monoid

data Identity a = Identity a

instance Foldable Identity where
  foldr f z (Identity x) = f x z
  foldl f z (Identity x) = f z x
  foldMap f (Identity x) = f x

data Optional a = Some a | Nada

instance Foldable Optional where
  foldr _ z Nada = z
  foldr f z (Some x) = f x z

  foldMap _ Nada = mempty
  foldMap f (Some x) = f x

mySum :: (Foldable t, Num a) => t a -> a
mySum = getSum . foldMap Sum

myProduct :: (Foldable t, Num a) => t a -> a
myProduct = getProduct . foldMap Product

myElem :: (Foldable t, Eq a) => a -> t a -> Bool
myElem e = getAny . foldMap (Any . (e ==))

myMinimum :: (Foldable t, Ord a) => t a -> Maybe a
myMinimum = foldr f Nothing
  where f e Nothing = Just e
        f e (Just x) = Just (min e x)

myMaximum :: (Foldable t, Ord a) => t a -> Maybe a
myMaximum = foldr f Nothing
  where f e Nothing = Just e
        f e (Just x) = Just (max e x)

myNull :: (Foldable t) => t a -> Bool
myNull = foldr f True
  where f _ _ = False

myLength :: (Foldable t) => t a -> Int
myLength = getSum <$> foldMap (const (Sum 1))

myToList :: (Foldable t) => t a -> [a]
myToList = foldr (:) []

myFold :: (Foldable t, Monoid m) => t m -> m
-- myFold = foldr mappend mempty
myFold = foldMap id

myFoldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
myFoldMap fm = foldr f mempty
  where f x z = fm x `mappend` z


data Constant a b = Constant a

instance Foldable (Constant a) where
  foldr _ z _ = z

data Two a b = Two a b

instance Foldable (Two a) where
  foldr f z (Two _ y) = f y z

data Three a b c = Three a b c

instance Foldable (Three a b) where
  foldr f z (Three _ _ x) = f x z

data Three' a b = Three' a b b

instance Foldable (Three' a) where
  foldr f z (Three' _ _ x) = f x z

data Four' a b = Four' a b b b

instance Foldable (Four' a) where
  foldr f z (Four' _ _ _ x) = f x z

filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF p = foldMap f
  where f x = if p x then pure x else mempty

main = print $ ((filterF even [1..10]) :: [Int])
