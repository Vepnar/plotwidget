module Main (main) where

import Data.Random
import Brick  (simpleMain)
import Control.Monad (replicateM)
import System.Random.MWC (createSystemRandom)

import Brick.Widgets.Plot (candleWidget,Candle(..))

constructCandle :: [Double] -> [Candle]
constructCandle [] = []
constructCandle xs = candle : constructCandle (drop 10 xs)
  where 
    candle = Candle (head x) (last x) (maximum x) (minimum x)
    x = take 11 xs

main :: IO ()
main = do
    mwc <- createSystemRandom
    diff <- runRVar (replicateM 5000 (normal 0 1)) mwc
    let price = foldl (\(x:xs) s -> (x+s):x:xs) [0] diff 
        cds = constructCandle price
    simpleMain $ candleWidget cds