module Chapter4 where

data Mood = Blah | Woot
  deriving Show

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood _    = Blah

main = print "Sup"

-- Exercises

awesome = ["Papuchon", "curry", ":)"]
alsoAwesome = ["Quake", "The Simons"]
allAwesome = [awesome, alsoAwesome]

-- 1)
myLength :: [a] -> Integer
myLength [] = 0
myLength (x:xs) = myLength xs + 1


-- 2)
--  a = 5
--  b = 3
--  c = 2
--  d = 5

-- 3)
-- It doesn't work because the result of myLength is an Integer and
-- there is no instance for Fractional Integer

-- 4)
-- Use `div` from the Integral typeclass

-- 5)
-- Bool, True

-- 6)
-- Bool, False

-- 7)
--    Works; True
--    Doesn't work. myLength argument is a List. List constituents should be homogeneous.
--    Works; 5
--    Works; False
--    Doesn't work; Second argument for logical conjunction must be a Bool

-- 8)
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

-- 9)
myAbs :: (Num a, Ord a) => a -> a
myAbs x = if x < 0 then negate x else x

-- 10)
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))

-- (Data declaration) (Type constructor) = (Data constructor)

-- POlymorphism in Haskell is either Parametric or Constrained...
