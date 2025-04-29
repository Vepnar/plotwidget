module Brick.Widgets.Plot.Types (
    Point,
    MIndex,
    Pixel(..),
    Candle(..),
    Dimensions(..),
    Option(..),
    Canvas(..),
    CanvasState
)
where

import Control.Monad.State

import Data.Bits ((.|.))
import Data.Char (chr)
import Data.Word (Word8)
import qualified Graphics.Vty as VT
import qualified Data.Vector as V
import qualified Data.Semigroup as Sem

type Point = (Double,Double)
type MIndex = (Int,Int)

data Pixel 
    = Colored { color :: VT.Color, char :: Char } 
    | Braille { color :: VT.Color, braille :: Word8 } 
    | Empty
    deriving (Eq)

data Candle = Candle 
    { candleOpen :: Double
    , candleClose :: Double
    , candleHigh :: Double
    , candleLow :: Double
    } deriving (Show, Eq)

instance Sem.Semigroup Candle where
    Candle o1 _ h1 l1 <> Candle _ c2 h2 l2 = Candle o1 c2 (max h1 h2) (min l1 l2)

instance Monoid Candle where
    mempty = Candle 0 0 0 0
    mappend = (Sem.<>)

data Dimensions = Dims
  { xMin :: Double
  , xMax :: Double
  , yMin :: Double
  , yMax :: Double
  } deriving (Show, Eq)

data Option = 
    Marker Char
    | Dimensions Dimensions
    | Primary VT.Color 
    | Secondary VT.Color
    deriving (Show, Eq)

instance Sem.Semigroup Pixel where
    Colored c1 c2 <> _ = Colored c1 c2
    Braille c1 b1 <> Braille c2 b2 
        | c1 == c2 = Braille c1 (b1 .|. b2)
        | otherwise = Braille c1 b1
    Braille c b <> _ = Braille c b
    Empty <> x = x

instance Monoid Pixel where
    mempty = Empty
    mappend = (Sem.<>)

instance Show Pixel where
    show (Colored _ c) = [c]
    show (Braille _ b) = [chr (fromIntegral b + 0x2800)]
    show Empty = " "

data Canvas = Canvas 
  { pixels :: V.Vector Pixel
  , width  :: Int
  , height :: Int
} deriving (Eq)

instance Show Canvas where
    show c = unlines [concatMap show (V.toList (V.slice (i * w) w pix)) | i <- [0 .. h - 1]]
        where 
            pix = pixels c
            w = width c
            h = height c

type CanvasState a = State Canvas a
