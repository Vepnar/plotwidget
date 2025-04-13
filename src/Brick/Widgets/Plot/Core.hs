module Brick.Widgets.Plot.Core (
    plot,
    paint,
    computeDimensions,

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

computeDimensions :: [Point] -> Dimensions
computeDimensions xs = Dims xm xM ym yM
  where 
    (xm,xM,ym,yM) = (minimum x, maximum x,minimum y, maximum y)
    x = map fst xs
    y = map snd xs

plot :: Int -> Int -> Canvas
plot w h 
  | h < 1 || w < 1 = error "plot: Invalid canvas shape"
  | otherwise =Canvas (V.replicate (w*h) Empty) w h


paint :: Canvas -> Widget n
paint (Canvas px w _) = let 
    paintRow xs
      | null xs = VT.emptyImage
      | otherwise = VT.horizCat (V.toList $ V.map img $ V.take w xs) <-> paintRow (V.drop w xs)
    in Widget Fixed Fixed $ return $ Result (paintRow px) [] [] [] Brick.BorderMap.empty

area :: Dimensions ->  Pixel -> [Point] -> CanvasState Dimensions
area d p xs = state $ \c -> (d, update c)
  where 
    uP c = uniqueFstMaxSnd (mapMaybe (matrixIndex c d) xs )
    update c@(Canvas px w h) = runST $ do
      v <- V.thaw px
      forM_ (uP c) $ \(x, y) -> 
              forM_ [y..(h-1)] $ \i -> MV.modify v  (p <>) (x + i * w)
      frz <- V.unsafeFreeze v
      return (Canvas frz w h)
      
area' :: Pixel -> [Point] -> CanvasState Dimensions
area' p xs = area d p xs
  where d = computeDimensions xs

scatter :: Dimensions -> Pixel -> [Point] -> CanvasState Dimensions
scatter d p xs = state $ \c -> (d, update c)
  where
    update c = runST $ do
      v <- V.thaw (pixels c)
      forM_ (mapMaybe (toIndex c d) xs) $ \x -> MV.modify v (p <>) x  
      frz <- V.unsafeFreeze v
      return (Canvas frz (width c) (height c))

scatter' :: Pixel -> [Point] -> CanvasState Dimensions
scatter' p xs = scatter d p xs
  where d = computeDimensions xs
