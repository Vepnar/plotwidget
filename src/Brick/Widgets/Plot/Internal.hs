module Brick.Widgets.Plot.Internal (
  matrixIndex,
  toIndex,
  scale,
  img,
  validDimensions,
  inDimensions,
  groupFst,
  uniqueFstMaxSnd,
 )
where

import qualified Data.Vector as V
import qualified Data.Vector as VM
import qualified Graphics.Vty as VT

import Brick.Widgets.Plot.Types

import Data.List (groupBy,maximumBy,sortBy,unfoldr)

import Data.Ord (comparing)
import Data.Maybe

scale :: (RealFrac a) => a -> a -> Int -> a -> a
scale vm vM s x
    | vm == vM = error "scale: minVal and maxVal cannot be equal (division by zero)"
    | otherwise = (x - vm) * fromIntegral s / (vM - vm)

img :: Pixel -> VT.Image
img Empty = VT.char VT.defAttr ' '
img (Colored color char) = VT.char attr char
  where attr = VT.defAttr {VT.attrForeColor = VT.SetTo color}
img (Natural char) = VT.char VT.defAttr char


-- Maybe remove
unsafeMatrixIndex :: Canvas -> Dimensions -> Point -> (Int,Int)
unsafeMatrixIndex (Canvas _ w h) (Dims xm xM ym yM) (x,y) = (xP,yP)
  where 
    xP = round $ scale xm xM (w-1) x :: Int
    yP = round $ scale yM ym (h-1) y :: Int

matrixIndex :: Canvas -> Dimensions -> Point -> Maybe (Int, Int)
matrixIndex c dims p
  | not $ validDimensions dims = Nothing
  | not $ inDimensions dims p = Nothing
  | otherwise = Just (unsafeMatrixIndex c dims p)

toIndex :: Canvas -> Dimensions -> Point -> Maybe Int
toIndex c@(Canvas _ w _) d p = 
    fmap (\(xP, yP) -> xP + w * yP) (matrixIndex c d p)

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