module Brick.Widgets.Plot.Internal (
  matrixIndex,
  scale,
  img,
  index,
  validDimensions,
  inDimensions,
  groupFst,
  uniqueFstMaxSnd,
  getDimensions,
  computeDimensions,
  getMarker,
  getPrimary,
  candlePixel,
  updatePixels,
  getUpperSymbol,
  getLowerSymbol,
  scaleCandles,
  toBraille,
  toCharPixel,
  toBraillePixel,
 )
where

import qualified Graphics.Vty as VT
import qualified Data.Vector as V

import Data.Word (Word8)
import Brick.Widgets.Plot.Types

import Data.List (groupBy,maximumBy,sortBy)

import Data.Ord (comparing)

scale :: (RealFrac a) => a -> a -> Int -> a -> a
scale vm vM s x
    | vm == vM = error "scale: minVal and maxVal cannot be equal (division by zero)"
    | otherwise = (x - vm) * fromIntegral s / (vM - vm)

img :: Pixel -> VT.Image
img Empty = VT.char VT.defAttr ' '
img (Colored co cr) = VT.char attr cr
  where attr = VT.defAttr {VT.attrForeColor = VT.SetTo co}
img b@(Braille co _) = VT.char attr $ show b !! 0
  where 
    attr = VT.defAttr {VT.attrForeColor = VT.SetTo $ color b}
    

computeDimensions :: [Point] -> Dimensions
computeDimensions [] = Dims 0 1 0 1
computeDimensions [(x,y)] = Dims (x-1) (x+1) (y-1) (y+1) 
computeDimensions xs = Dims xm xM ym yM
  where 
    (xm,xM,ym,yM) = (minimum x, maximum x,minimum y, maximum y)
    x = map fst xs
    y = map snd xs

getDimensions :: [Option] -> Maybe Dimensions
getDimensions [] = Nothing
getDimensions (Dimensions d:_) = Just d
getDimensions (_:xs) = getDimensions xs

getMarker :: [Option] -> Maybe Char
getMarker [] = Nothing
getMarker (Marker m:_) = Just m
getMarker (_:xs) = getMarker xs

getPrimary :: [Option] -> Maybe VT.Color
getPrimary [] = Nothing
getPrimary (Primary c:_) = Just c
getPrimary (_:xs) = getPrimary xs

-- Study optics
updatePixels :: Canvas -> V.Vector Pixel -> Canvas
updatePixels (Canvas _ w h) v' = Canvas v' w h

index :: Canvas -> Dimensions -> Point -> Maybe Point
index c d@(Dims xm xM ym yM) (x,y)
  | not $ validDimensions d = Nothing
  | not $ inDimensions d (x,y) = Nothing
  | otherwise = Just (xP,yP)
  where 
    xP = scale xm xM (width c-1) x
    yP = scale yM ym (height c-1) y

matrixIndex :: Canvas -> Dimensions -> Point -> Maybe (Int, Int)
matrixIndex c d = fmap (\(x,y) -> (round x, round y)) . index c d

toBraille :: Point -> Word8
toBraille (x,y) 
  | cx > 0.5 && cy > 0.75 = 0x01
  | cx <= 0.5 && cy > 0.75 = 0x08
  | cx > 0.5 && cy > 0.50 = 0x02
  | cx <= 0.5 && cy > 0.50 = 0x10
  | cx > 0.5 && cy > 0.25 = 0x04
  | cx <= 0.5 && cy > 0.25 = 0x20
  | cx > 0.5 = 0x40
  | cx <= 0.5 = 0x80
  where 
    cx = abs (x - fromIntegral (floor x))
    cy = abs (y - fromIntegral (floor y))

toBraillePixel :: VT.Color -> (Double,Double) -> (MIndex,Pixel)
toBraillePixel c (x,y) = if b == 0 then (p, Empty) else (p, Braille c b)
  where 
    p = (round x,round y)
    b = toBraille (x,y)

toCharPixel :: VT.Color -> Char -> (Double,Double) -> (MIndex, Pixel)
toCharPixel color ch (x,y) = (p, Colored color ch)
  where 
    p = (round x,round y) :: (Int,Int)

validDimensions :: Dimensions -> Bool
validDimensions (Dims xm xM ym yM) = xm < xM && ym < yM

inDimensions :: Dimensions -> Point -> Bool
inDimensions dims@(Dims xm xM ym yM) (x, y)
  | not (validDimensions dims) = error "inDimensions: Invalid dimensions"
  | otherwise =  x >= xm && x <= xM && y >= ym && y <= yM

groupFst :: Ord a => [(a, b)] -> [[(a, b)]]
groupFst = groupBy (\(x,_) (y,_) -> x == y) . sortBy (comparing fst)

uniqueFstMaxSnd :: (Ord a, Ord b) => [(a,b)] -> [(a,b)]
uniqueFstMaxSnd = map (maximumBy (comparing snd)) . groupFst

scaleCandles :: Int -> [Candle] -> [Candle]
scaleCandles h candles = map scaleCandle candles
  where
    scaleCandle (Candle op cl hi lo) =
      Candle (scaler op) (scaler cl) (scaler hi) (scaler lo)
    scaler = scale ym yM h
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
    ym = min op cl
    yM = max op cl
    y' = fromIntegral y :: Double
    pix = Colored $ if cl < op then VT.green else VT.red 