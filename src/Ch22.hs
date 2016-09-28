module Ch22 where

import Data.Char
import Control.Applicative
import Data.Maybe

cap :: String -> String
cap = fmap toUpper

rev :: String -> String
rev = reverse

tupled :: String -> (String, String)
-- tupled = do
--   c <- cap
--   r <- rev
--   const (c, r)
tupled = (,) <$> cap <*> rev

newtype Reader r a =
  Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap f (Reader ra) =
    Reader $ f . ra

ask :: Reader a a
ask = Reader id

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f fa fb = f <$> fa <*> fb

asks :: (r -> a) -> Reader r a
asks = Reader

instance Applicative (Reader r) where
  pure = Reader . const
--   (Reader ff) <*> (Reader fa) = Reader (ff <*> fa)
  (Reader ff) <*> (Reader fa) = Reader $ \x -> ff x (fa x)

instance Monad (Reader r) where
  return = pure

  (Reader fa) >>= f = Reader (\x -> let a = fa x in runReader (f a) x)

-------------------------------------------------------

-- x = [1..3]
-- y = [4..6]
-- z = [7..9]
--
-- lookup :: Eq a => a -> [(a, b)] -> Maybe b
-- lookup x = foldr go Nothing
--   where go (key, elem) z = if key == x then Just elem else z

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
bolt = (&&) <$> (>3) <*> (<8)



main = print "22"
