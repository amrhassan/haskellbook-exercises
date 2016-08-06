module LensesWorksheet where

import Control.Lens hiding (element)

data Point = Point { _x :: Double, _y :: Double }
  deriving Show

data Atom = Atom { _element :: String, _point :: Point }
  deriving Show


element :: Lens' Atom String
element = undefined

point :: Lens' Atom Point
point = undefined

x :: Lens' Point Double
x = undefined

y :: Lens' Point Double
y = undefined

shiftAtomX :: Atom -> Atom
-- shiftAtomX (Atom e (Point x y)) = Atom e (Point (x + 1) y)
shiftAtomX = over (point . x) (+1)

myatom = Atom "atomic" (Point 0.3 0.4)

main = print $ myatom
