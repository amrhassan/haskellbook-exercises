module Chapter10 where

import Data.Time

data DatabaseItem = DbString String
  | DbNumber Integer
  | DbDate UTCTime
  deriving (Eq, Ord, Show)


theDatabase :: [DatabaseItem]
theDatabase = [
  DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123)),
  DbNumber 9001,
  DbString "Hello DB",
  DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))]


filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = concatMap f
  where f :: DatabaseItem -> [UTCTime]
        f (DbDate t) = [t]
        f _ = []

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = concatMap (\x -> case x of
  DbNumber i -> [i]
  _ -> [])

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb is = (fromIntegral (sumDb is)) / fromIntegral (length is)

fibs = 1 : scanl (+) 1 fibs
-- fact = 1 : 1 : scanl (*) 2 fact

fact :: Integer -> Integer
fact 1 = 1
fact n = n * fact (n - 1)

-- facts = map fact [1..]
facts = map fst numbered
  where numbered = (1, 1) : map (\(x, i) -> (x * (i+1), i+1)) numbered

f 1 = 1                   -- 1
f 2 = 1 * 2               -- 2
f 3 = 1 * 2 * 3           -- (6, 3)
f 4 = 1 * 2 * 3 * 4       -- 24

stops = "pbtdkg"
vowels = "aeiou"

-- 1.a
svsWords = [(s1, v, s2) | s1 <- stops, v <- vowels, s2 <- stops]

-- 1.b
svsWordsP = filter (\x -> head x == 'p') svsWords

-- Average word length


main = print $ take 10 facts
