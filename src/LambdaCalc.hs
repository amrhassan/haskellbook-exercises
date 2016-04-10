module LambdaCalc where

-- A function that takes in a successor function and a zero value and returns a representation of the number
type Nat i = (i -> i) -> i -> i

zero :: Nat a
zero _ z = z

-- Successor of a Number z
-- \wyx.y(wyx)
nsucc :: Nat z -> Nat z
nsucc a s z = s (a s z)

-- Addition
-- Idea: treat `b` as the new zero for the application of `a`
-- \ab.a(\wyx.y(wyx))b
plus :: Nat z -> Nat z -> Nat z
plus a b s z = a s (b s z)

-- Multiplication
-- Idea: Use `b` as the successor for evaluating `a`
-- \abz.a(bz)
times :: Nat z -> Nat z -> Nat z
times a b s z = a (b s) z

one = nsucc zero
two = nsucc one
three = nsucc two
four = two `plus` two
five = three `plus` two
seven = five `plus` two
nine = four `plus` five
twentyone = three `times` seven

toNum :: (Enum n, Num n) => Nat n -> n
toNum n = n succ 0

------------------------------------------------------------------------------------------------------------------------

type Boolean a = a -> a -> a

true :: Boolean a
true x y = x

false :: Boolean a
false x y = y

-- And / Conjunction
band :: Boolean a -> Boolean a -> Boolean a
band a b x y = a (b x y) (false x y)

-- Or / Disjunction
bor :: Boolean a -> Boolean a -> Boolean a
bor a b x y = a (true x y) (b x y)

-- Not / Negation
bnot :: Boolean a -> Boolean a
bnot a x y = a (false x y) (true x y)

isZero :: Nat (Boolean b) -> Boolean b
isZero n = n (const false) true

toBool :: Boolean Bool -> Bool
toBool b = b True False

-- TODO: predecessor and recursion
