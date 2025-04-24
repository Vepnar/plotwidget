module Main (main) where

import Data.Random
import Brick  (simpleMain)
import Control.Monad (replicateM)
import System.Random.MWC (createSystemRandom)

import Brick.Widgets.Plot (candleWidget,Candle(..))

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n l
  | n > 0 = (take n l) : (chunksOf n (drop n l))
  | otherwise = error "Negative or zero n"

constructCandle :: [Double] -> Candle 
constructCandle xs = Candle (head xs) (last xs) (maximum xs) (minimum xs)

main :: IO ()
main = do
    mwc <- createSystemRandom
    nSamples <- runRVar (replicateM 9999 (normal 0 1)) mwc
    let price = foldl (\(x:xs) s -> (x+s):x:xs) [0] nSamples 
        cds = map (constructCandle) (chunksOf 10 price)
    simpleMain $ candleWidget cds