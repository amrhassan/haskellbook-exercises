module Ch23 where

import System.Random
import Control.Monad.Trans.State
import Control.Applicative
import Control.Monad
import Control.Arrow

data Die =
  DieOne |
  DieTwo |
  DieThree |
  DieFour |
  DieFive |
  DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n = case n `mod` 6 of
  0 -> DieOne
  1 -> DieTwo
  2 -> DieThree
  3 -> DieFour
  4 -> DieFive
  5 -> DieSix

rollDie :: State StdGen Die
rollDie = intToDie <$> state random

rollDies :: Int -> [Die]
rollDies n = evalState (replicateM n rollDie) (mkStdGen 0)

---------------------------------

newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap f (Moi g) = Moi (\s -> let (x, ns) = g s in (f x, ns))

instance Applicative (Moi s) where
  pure x = Moi (\s -> (x, s))
  (Moi ff) <*> (Moi fx) = Moi (\s -> let
    (f, s') = ff s
    (x, s'') = fx s' in
      (f x, s''))

instance Monad (Moi s) where
  return = pure
  (Moi fa) >>= g = Moi (\s ->
    let (a, s') = fa s in
      runMoi (g a) s')

----------------------------------

data FizzBuzz = Fizz | Buzz | FizzBuzz | Neither Int
  deriving (Show)

fizzBuzz :: Int -> FizzBuzz
fizzBuzz n
  | n `mod` 15 == 0 = FizzBuzz
  | n `mod` 3 == 0 = Fizz
  | n `mod` 5 == 0 = Buzz
  | otherwise = Neither n

incMoi :: (Enum s) => (s -> a) -> Moi s a
incMoi f = Moi $ f &&& succ

fizzBuzzes :: Moi Int FizzBuzz
fizzBuzzes = incMoi fizzBuzz

-----------------------------------

moiGet :: Moi s s
moiGet = Moi (\s -> (s, s))

moiPut :: s -> Moi s ()
moiPut x = Moi (const ((), x))

moiRunS :: Moi s a -> s -> s
moiRunS (Moi f) s = let (_, ns) = f s in ns

moiRunA :: Moi s a -> s -> a
moiRunA (Moi f) s = let (a, _) = f s in a

moiModify :: (s -> s) -> Moi s ()
moiModify sf = do
  s <- moiGet
  moiPut (sf s)

-- main = print $ runMoi (moiModify (+1) >> moiModify (+1)) 0

main = print $ rollDies 5
