module Brick.Widgets.Plot.Types (
    Dimensions(..),
    Canvas(..),
    Point(..),
    PixelData(..),
    Pixel,
) where

import qualified Graphics.Vty as VT
import qualified Data.Vector as V
import qualified Data.Semigroup as Sem

type Point = (Double,Double)
type Pixel = Maybe (VT.Color, Char)

data PixelData 
    = Colored { color :: VT.Color, char :: Char } 
    | Natural { char :: Char } 
    | Empty
    deriving (Eq)

toPixel :: PixelData -> Pixel
toPixel Empty = Nothing
toPixel (Colored c ch) = Just (c, ch)
toPixel (Natural ch) = Just (VT.white, ch)


instance Sem.Semigroup PixelData where
    _ <> Colored c1 c2 = Colored c1 c2
    _ <> Natural c = Natural c
    Empty <> x = x
    x <> Empty = x  

instance Monoid PixelData where
    mempty = Empty
    mappend = (Sem.<>)

instance Show PixelData where
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
  { pixels :: V.Vector PixelData
  , width  :: Int
  , height :: Int
}