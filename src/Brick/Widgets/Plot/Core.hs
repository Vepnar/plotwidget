module Brick.Widgets.Plot.Core (
    plot,
    paint,
    scatter,
    scatter',
    area,
    area',
    candle,
    drawPixels,
)

where

import Brick.Types
import Brick.BorderMap
import Data.Maybe

import qualified Data.Vector as V
import qualified Graphics.Vty as VT
import qualified Data.Vector.Mutable as MV

import Control.Monad (forM_)
import Control.Monad.ST (runST)
import Control.Monad.State

import Brick.Widgets.Plot.Internal
import Brick.Widgets.Plot.Types

import Graphics.Vty ((<->))

plot :: Int -> Int -> Canvas
plot w h 
  | h < 1 || w < 1 = error "plot: Invalid canvas shape"
  | otherwise = Canvas (V.replicate (w*h) Empty) w h

paint :: Canvas -> Widget n
paint (Canvas px w _) = let 
    paintRow xs
      | null xs = VT.emptyImage
      | otherwise = VT.horizCat (V.toList $ V.map img $ V.take w xs) <-> paintRow (V.drop w xs)
    in Widget Fixed Fixed $ return $ Result (paintRow px) [] [] [] Brick.BorderMap.empty

drawPixels :: [(MIndex, Pixel)] -> CanvasState ()
drawPixels [] = return ()
drawPixels xs = state $ \c -> ((), update c)
  where 
    update c = runST $ do
      v <- V.thaw (pixels c)
      forM_ xs $ \((x,y),p) -> MV.modify v (p<>) (x + (width c) * y)
      frz <- V.unsafeFreeze v
      return $ updatePixels c frz

area :: [Option] -> [Point] -> CanvasState Dimensions
area opt xs = state $ \c -> (dims, update c)
  where 
    pix = getMarker opt
    dims = getDimensions opt xs
    uP c = uniqueFstMaxSnd (mapMaybe (matrixIndex c dims) xs )
    update c = runST $ do
      v <- V.thaw $ pixels c
      forM_ (uP c) $ \(x, y) -> 
              forM_ [y..((height c)-1)] $ \i -> MV.modify v  (pix <>) (x + i * (width c))
      frz <- V.unsafeFreeze v
      return $ updatePixels c frz

area' :: [Point] -> CanvasState Dimensions
area' = area []

scatter :: [Option] -> [Point] -> CanvasState Dimensions
scatter opt xs = state $ \c -> (dims, update c)
  where
    pix = getMarker opt
    dims = getDimensions opt xs
    update c = runST $ do
      v <- V.thaw (pixels c)
      forM_ (mapMaybe (toIndex c dims) xs) $ \x -> MV.modify v (pix <>) x  
      frz <- V.unsafeFreeze v
      return $ updatePixels c frz

scatter' :: [Point] -> CanvasState Dimensions
scatter' = scatter []

combineCandles :: [Candle] -> Candle
combineCandles [] = error "combineCandles: empty list"
combineCandles cs = Candle
  { candleOpen = candleOpen (head cs)
  , candleClose = candleClose (last cs)
  , candleHigh = maximum (map candleHigh cs)
  , candleLow = minimum (map candleLow cs)
  }

constructCandle :: [Point] -> Candle
constructCandle [] = error "constructCandle: empty list"
constructCandle ps = Candle
  { candleOpen = snd (head ps)
  , candleClose = snd (last ps)
  , candleHigh = maximum $ map snd ps
  , candleLow = minimum $ map snd ps
  }

candle :: [Candle] -> CanvasState ()
candle cs = state $ \canvas -> ((), update canvas)
  where
    scaledCds c = scaleCandles c $ takeLastN (width c) cs
    update c = runST $ do
      v <- V.thaw (pixels c)
      forM_ (zip [0..] $ scaledCds c) $ \(x, cd) ->
        forM_ [0 .. height c - 1] $ \y ->
          let pixelIndex = x + y * width c
              pixelValue = candlePixel (height c - y) cd
          in MV.modify v (pixelValue <>) pixelIndex
      frz <- V.unsafeFreeze v
      return $ updatePixels c frz
