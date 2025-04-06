module Brick.Widgets.Plot.Core (
    plot,
    paint,
    computeDimensions,

    scatter,
    scatter',
    scatter'',

    area,
    area',
    area'',
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
  | otherwise = Canvas (V.replicate (w*h) Empty) w h 

paint :: Canvas -> Widget n
paint (Canvas pix w _) = let 
    paintRow px
      | null px = VT.emptyImage
      | otherwise = VT.horizCat (img <$> ll) <-> paintRow r
      where 
        ll = V.toList l
        (l, r) = V.splitAt w px 
    in Widget Fixed Fixed $ do 
      return $ Result (paintRow pix) [] [] [] Brick.BorderMap.empty

area :: Dimensions -> Pixel -> [Point] -> Canvas -> Canvas
area d p xs c@(Canvas px w h) = runST $ do
  v <- V.unsafeThaw px
  forM_ uP $ \(x, y) -> 
          forM_ [y..(h-1)] $ \i -> MV.modify v  (p <>) (coords x i)
  frz <- V.unsafeFreeze v
  return $ Canvas frz w h
  where
    coords x y = x + w * y
    uP = uniqueFstMaxSnd (mapMaybe (matrixIndex c d) xs )

area' :: Pixel -> [Point] -> Canvas -> Canvas
area' p xs = area d p xs
  where 
    d = computeDimensions xs

area'' :: [Point] -> Canvas -> Canvas
area'' = area' whiteStar

scatter :: Dimensions -> Pixel -> [Point] -> Canvas -> Canvas
scatter d p xs c@(Canvas px w h) = runST $ do
  v <- V.unsafeThaw px
  forM_ (mapMaybe (toIndex c d) xs) $ \x -> MV.modify v (p <>) x  
  frz <- V.unsafeFreeze v
  return $ Canvas frz w h

scatter' :: Pixel -> [Point] -> Canvas -> Canvas
scatter' p xs c = scatter d p xs c
    where 
        d = computeDimensions xs

scatter'' :: [Point] -> Canvas -> Canvas
scatter'' xs c = scatter' whiteStar xs c