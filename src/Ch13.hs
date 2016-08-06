module Ch13 where

import System.IO
import Control.Monad
import refined

palindrome :: IO ()
palindrome = forever $ do
  putStrLn "Enter text:"
  line1 <- getLine
  if line1 == reverse line1 then
    putStrLn "It's a palindrome!"
  else
    putStrLn "Nope!"

type Name = String
type Age = Integer

data Person = Person Name Age
  deriving Show

data PersonInvalid = NameEmpty | AgeTooLow | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age >0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown $ "Name was: " ++ show name ++ " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  name <- ((putStrLn "Enter name: ") >> getLine) :: Refined
  age <- ((putStrLn "Enter age: ") >> getLine)
  case (mkPerson name (read age)) of
    (Right person) -> putStrLn ("Yay! " ++ show person)
    (Left e) -> putStrLn ("Failure: " ++ show e)

main = gimmePerson
