module Brick.Widgets.Plot.Core (
    plot,
    paint,
    scatter,
    scatter',
    area,
    area',
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
