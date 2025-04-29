module Brick.Widgets.Plot.Core (
    plot,
    scatter,
    scatter',
    area,
    area',
    candle,
    drawPixels,
    toWidget,
    toStr,
    toConsole,
)

where

import Brick.Types
import Brick.BorderMap
import Data.Maybe

import Control.Applicative ((<|>))
import qualified Data.Vector as V
import qualified Graphics.Vty as VT
import qualified Data.Vector.Mutable as MV

import Control.Monad (forM_, ap)
import Control.Monad.ST (runST)
import Control.Monad.State

import Brick.Widgets.Plot.Internal
import Brick.Widgets.Plot.Types

import Graphics.Vty ((<->))

plot :: Int -> Int -> Canvas
plot w h 
  | h < 1 || w < 1 = error "plot: Invalid canvas shape"
  | otherwise = Canvas (V.replicate (w*h) Empty) w h

toStr :: Int -> Int -> CanvasState a -> String
toStr w h cs = show $ execState cs $ plot w h

toConsole :: Int -> Int -> CanvasState a -> IO ()
toConsole w h = putStr . toStr w h

toWidget :: Int -> Int ->  CanvasState a -> Widget n
toWidget w h cs = let
  canvas = execState cs $ plot w h
  paintRow xs
    | null xs = VT.emptyImage
    | otherwise = VT.horizCat (V.toList $ V.map img $ V.take w xs) <-> paintRow (V.drop w xs)
  in Widget Fixed Fixed $ return $ Result (paintRow $ pixels canvas) [] [] [] Brick.BorderMap.empty

drawPixels :: [(MIndex, Pixel)] -> CanvasState ()
drawPixels [] = return ()
drawPixels xs = state $ \c -> ((), update c)
  where 
    update c = runST $ do
      v <- V.thaw (pixels c)
      forM_ xs $ \((x,y),p) -> MV.modify v (p<>) (x + (width c) * y)
      frz <- V.unsafeFreeze v
      return $ updatePixels c frz

-- Implement braille
area :: [Option] -> [Point] -> CanvasState Dimensions
area opt xs = do
  c <- get
  let marker = fromMaybe '*' $ getMarker opt
      col = fromMaybe VT.white $ getPrimary opt
      dims = fromMaybe (computeDimensions xs) $ getDimensions opt 
      uP = uniqueFstMaxSnd $ mapMaybe (matrixIndex c dims) xs
      pos = uP >>= \(x,y) -> [(x,i) | i <- [y..(height c) - 1]]
  drawPixels $ zip pos $ repeat $ Colored col marker
  return dims

area' :: [Point] -> CanvasState Dimensions
area' = area []

scatter :: [Option] -> [Point] -> CanvasState Dimensions
scatter opt xs = do
  cv <- get
  let marker = getMarker opt
      col = getPrimary opt <|> Just VT.white
      dims = fromMaybe (computeDimensions xs) $ getDimensions opt 
      scaledXs = map (index cv dims) xs
      charDrawer = liftA2 toCharPixel col marker
      brailleDrawer = toBraillePixel <$> col
      drawer = brailleDrawer <|> charDrawer
  drawPixels $ mapMaybe (ap drawer) scaledXs
  return dims
    
scatter' :: [Point] -> CanvasState Dimensions
scatter' = scatter []

-- Implement option handling
candle :: [Candle] -> CanvasState Dimensions
candle cs = do 
  c <- get 
  let (w, h) = (width c, height c) 
      cs' = scaleCandles h $ take w $ reverse cs
      (ym, yM) = (minimum $ map candleLow cs', maximum $ map candleHigh cs')
      column = [0..h-1]
  drawPixels $ (zip [0..]cs') >>= \(x, cd) -> 
    [((x,y), (candlePixel (h - y) cd)) | y <- column]
  return (Dims 0 (fromIntegral w) ym yM)