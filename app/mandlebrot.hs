module Main (main) where

import Brick 
import Data.Complex
import Lens.Micro ((^.))

import Brick.Widgets.Plot

viewPort :: Dimensions
viewPort = Dims (-2.5) (1.0) (-1) (1)

mandelbrot :: Int -> Complex Double -> Bool
mandelbrot maxIter c = iter 0 (0 :+ 0)
  where
    iter n z
      | n >= maxIter = True
      | magnitude (z^2 + c) > 2 = False
      | otherwise = iter (n+1) (z^2 + c)

mandelbrotSet :: Dimensions -> Int -> Int -> [(Double, Double)]
mandelbrotSet (Dims xm xM ym yM) w h = [(cx,cy) | x <- [0,0.5..w'-1], y <- [0,0.125..h'-1],
    let cx = x * (xM-xm) / w' + xm,
    let cy = y * (yM-ym) / h' + ym,
    mandelbrot 300 (cx :+ cy)
    ]
    where (w', h') = (fromIntegral w, fromIntegral h)

plotMandelbrotSet :: Widget ()
plotMandelbrotSet = Widget Greedy Greedy $ do
    ctx <- getContext
    let w = ctx^.availWidthL
        h = ctx^.availHeightL
    render $ toWidget w h $ scatter [Dimensions viewPort] $ mandelbrotSet viewPort w h

main :: IO ()
main = simpleMain plotMandelbrotSet
