module Brick.Widgets.Plot.Internal (
  matrixIndex,
  toIndex,
  scale,
  img,
  validDimensions,
  inDimensions,
  groupFst,
  uniqueFstMaxSnd,
  getDimensions,
  computeDimensions,
  getMarker,
  candlePixel,
  updatePixels,
  getUpperSymbol,
  getLowerSymbol,
  scaleCandles,
  takeLastN,
 )
where

import qualified Graphics.Vty as VT
import qualified Data.Vector as V

import Brick.Widgets.Plot.Types

import Data.List (groupBy,maximumBy,sortBy)

import Data.Ord (comparing)

-- | Scales a value from one range to another.
-- 
-- This function takes a value `x` in the range defined by `vm` (minimum value)
-- and `vM` (maximum value) and scales it to a new range defined by `s` (the
-- size of the target range). The result is the scaled value in the target range.
--
-- ==== __Parameters__
--
-- * @vm@: The minimum value of the input range.
-- * @vM@: The maximum value of the input range. Must not be equal to @vm@.
-- * @s@: The size of the target range (as an integer).
-- * @x@: The value to be scaled.
--
-- ==== __Returns__
--
-- The scaled value in the target range.
--
-- ==== __Errors__
--
-- Throws an error if @vm@ is equal to @vM@, as this would result in a division
-- by zero.
scale :: (RealFrac a) => a -> a -> Int -> a -> a
scale vm vM s x
    | vm == vM = error "scale: minVal and maxVal cannot be equal (division by zero)"
    | otherwise = (x - vm) * fromIntegral s / (vM - vm)

-- | Converts a 'Pixel' into a 'VT.Image' for rendering in a terminal.
-- 
-- The function handles three types of 'Pixel':
-- 
-- * 'Empty': Represents an empty space and is rendered as a blank character (' ').
-- * 'Colored': Represents a character with a specific foreground color. The color
--   is applied using the 'VT.attrForeColor' attribute.
-- * 'Natural': Represents a character with the default terminal attributes.
-- 
-- Arguments:
-- 
-- * 'Pixel': The input pixel to be converted.
-- 
-- Returns:
-- 
-- * 'VT.Image': The corresponding terminal image representation of the input pixel.
img :: Pixel -> VT.Image
img Empty = VT.char VT.defAttr ' '
img (Colored co cr) = VT.char attr cr
  where attr = VT.defAttr {VT.attrForeColor = VT.SetTo co}
img (Natural cr) = VT.char VT.defAttr cr

-- | Computes the dimensions of a bounding box that encompasses a list of points.
-- 
-- The function takes a list of 2D points (represented as tuples of x and y coordinates)
-- and returns a 'Dimensions' value that represents the bounding box.
-- 
-- * If the input list is empty, the function throws an error.
-- * If the input list contains a single point, the bounding box is defined as a small
--   area around the point, with a margin of 1 unit in all directions.
-- * For a list of multiple points, the bounding box is determined by the minimum and
--   maximum x and y coordinates of the points.
-- 
-- __Parameters:__
-- 
-- - @[Point]@: A list of 2D points, where each point is a tuple @(x, y)@.
-- 
-- __Returns:__
-- 
-- - @Dimensions@: A bounding box defined by the minimum and maximum x and y coordinates.
-- 
-- __Throws:__
-- 
-- - @error@: If the input list is empty.
computeDimensions :: [Point] -> Dimensions
computeDimensions [] = Dims 0 1 0 1
computeDimensions [(x,y)] = Dims (x-1) (x+1) (y-1) (y+1) 
computeDimensions xs = Dims xm xM ym yM
  where 
    (xm,xM,ym,yM) = (minimum x, maximum x,minimum y, maximum y)
    x = map fst xs
    y = map snd xs


getDimensions :: [Option] -> [Point] -> Dimensions
getDimensions [] xs = computeDimensions xs
getDimensions (Dimensions d:_) _ = d
getDimensions (_:os) xs = getDimensions os xs

getMarker :: [Option] -> Pixel
getMarker [] = whiteStar
getMarker (Marker m:_) = m
getMarker (_:xs) = getMarker xs

-- Study optics
updatePixels :: Canvas -> V.Vector Pixel -> Canvas
updatePixels (Canvas _ w h) v' = Canvas v' w h

matrixIndex :: Canvas -> Dimensions -> Point -> Maybe (Int, Int)
matrixIndex c d@(Dims xm xM ym yM) (x,y)
  | not $ validDimensions d = Nothing
  | not $ inDimensions d (x,y) = Nothing
  | otherwise = Just (xP,yP)
  where 
    xP = round $ scale xm xM (width c-1) x
    yP = round $ scale yM ym (height c-1) y

-- Maybe generalize
toIndex :: Canvas -> Dimensions -> Point -> Maybe Int
toIndex c d p = 
    fmap (\(xP, yP) -> xP + (width c) * yP) (matrixIndex c d p)

validDimensions :: Dimensions -> Bool
validDimensions (Dims xm xM ym yM) = xm < xM && ym < yM

inDimensions :: Dimensions -> Point -> Bool
inDimensions dims@(Dims xm xM ym yM) (x, y)
  | not (validDimensions dims) = error "inDimensions: Invalid dimensions"
  | otherwise =  x >= xm && x <= xM && y >= ym && y <= yM

groupFst :: Eq a => [(a, b)] -> [[(a, b)]]
groupFst = groupBy (\(x,_) (y,_) -> x == y)

uniqueFstMaxSnd :: (Ord a) => [(a,a)] -> [(a,a)]
uniqueFstMaxSnd = map (maximumBy (comparing snd)) . groupFst . sortBy (comparing fst)

scaleCandles :: Canvas -> [Candle] -> [Candle]
scaleCandles canvas candles = map scaleCandle candles
  where
    scaleCandle (Candle op cl hi lo) =
      Candle (scaler op) (scaler cl) (scaler hi) (scaler lo)
    scaler value = scale ym yM (height canvas) value
    ym = (minimum $ map candleLow candles)
    yM = (maximum $ map candleHigh candles)
    
getUpperSymbol :: Double -> Candle -> Char
getUpperSymbol y (Candle op cl hi _) 
  | mDiff > 0.75 = '┃'
  | mDiff > 0.25, hDiff > 0.75 = '╽'
  | mDiff > 0.25, hDiff <= 0.75 = '╻'
  | hDiff > 0.75 = '│'
  | hDiff > 0.25 = '╷'
  | otherwise = ' '
  where 
    mDiff = (max op cl) - y
    hDiff = hi - y

getLowerSymbol :: Double -> Candle -> Char
getLowerSymbol x (Candle op cl _ lo)
  | mDiff < 0.25 = '┃'
  | mDiff < 0.75 && lDiff < 0.25 = '╿'
  | mDiff < 0.75 && lDiff > 0.25 = '╹'
  | lDiff < 0.25 = '│' 
  | lDiff < 0.75 = '╵'
  | otherwise = ' '
  where 
    mDiff = (min op cl) - x
    lDiff = lo - x

candlePixel :: Int -> Candle -> Pixel
candlePixel y c@(Candle op cl hi lo)
  | ceiling hi >= y, y >= floor yM = pix $ getUpperSymbol y' c
  | ceiling ym >= y, y >= floor lo = pix $ getLowerSymbol y' c
  | yM >= y' && y >= ceiling ym = pix $ '┃'
  | otherwise = Empty
  where
    bullish = op < cl
    ym = min op cl
    yM = max op cl
    y' = fromIntegral y :: Double
    pix = Colored $ if bullish then VT.green else VT.red 

takeLastN :: Int -> [a] -> [a]
takeLastN n = reverse . take n . reverse