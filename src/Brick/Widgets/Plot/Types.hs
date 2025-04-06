module Brick.Widgets.Plot.Types where

import qualified Graphics.Vty as VT
import qualified Data.Vector as V
import qualified Data.Semigroup as Sem

type Point = (Double,Double)

data Pixel 
    = Colored { color :: VT.Color, char :: Char } 
    | Natural { char :: Char } 
    | Empty
    deriving (Eq)

instance Sem.Semigroup Pixel where
    Colored c1 c2 <> _ = Colored c1 c2
    Natural c <> _ = Natural c
    Empty <> x = x
    x <> Empty = x  

instance Monoid Pixel where
    mempty = Empty
    mappend = (Sem.<>)

instance Show Pixel where
    show (Colored _ c) = [c]
    show (Natural c) = [c]
    show Empty = " "

data Dimensions = Dims
  { xMin :: Double
  , xMax :: Double
  , yMin :: Double
  , yMax :: Double
  } deriving (Show, Eq)

data Canvas = Canvas 
  { pixels :: V.Vector Pixel
  , width  :: Int
  , height :: Int
}

-- Generated by AutoPilot
redStar = Colored VT.red '*'
blueStar = Colored VT.blue '*'
greenStar = Colored VT.green '*'
whiteStar = Natural '*'
yellowStar = Colored VT.yellow '*'
magentaStar = Colored VT.magenta '*'
cyanStar = Colored VT.cyan '*'

redDot = Colored VT.red '⋅'
blueDot = Colored VT.blue '⋅'
greenDot = Colored VT.green '⋅'
whiteDot = Colored VT.white '⋅'
yellowDot = Colored VT.yellow '⋅'
magentaDot = Colored VT.magenta '⋅'
cyanDot = Colored VT.cyan '⋅'

redCirc = Colored VT.red 'o'
blueCirc = Colored VT.blue 'o'
greenCirc = Colored VT.green 'o'
whiteCirc = Colored VT.white 'o'
yellowCirc = Colored VT.yellow 'o'
magentaCirc = Colored VT.magenta 'o'
cyanCirc = Colored VT.cyan 'o'

redHash = Colored VT.red '#'
blueHash = Colored VT.blue '#'
greenHash = Colored VT.green '#'
whiteHash = Colored VT.white '#'
yellowHash = Colored VT.yellow '#'
magentaHash = Colored VT.magenta '#'
cyanHash = Colored VT.cyan '#'
