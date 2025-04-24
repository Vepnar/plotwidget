module InternalSpec (spec) where 

import Test.Hspec
import Data.Maybe
import Control.Exception (evaluate)

import Brick.Widgets.Plot.Internal
import Brick.Widgets.Plot.Types
import Brick.Widgets.Plot.Core (plot)

spec :: Spec
spec = do
    describe "validDimensions" $ do
        it "xMin and xMax equal is not valid" $ 
            validDimensions (Dims 1 1 0 1) `shouldBe` False
        it "yMin and yMax equal is not valid" $ 
            validDimensions (Dims 0 1 1 1) `shouldBe` False
        it "xMin greater than xMax equal is not valid" $
            validDimensions (Dims 1 0 1 2) `shouldBe` False
        it "yMin greater than yMax equal is not valid" $
            validDimensions (Dims 2 4 4 2) `shouldBe` False
        it "Small dimensions are valid" $
            validDimensions (Dims 0 0.1 0 0.1) `shouldBe` True
        it "Large dimensions are valid" $
            validDimensions (Dims 10 1000 10 1000) `shouldBe` True

    describe "inDimensions" $ do
        it "Below xMin is external" $
            inDimensions (Dims 0 1 0 1) (-0.1, 0) `shouldBe` False
        it "At xMin is internal" $
            inDimensions (Dims 0 1 0 1) (0, 0) `shouldBe` True
        it "Above xMax is external" $
            inDimensions (Dims 0 1 0 1) (1.1, 0) `shouldBe` False
        it "At xMax is internal" $
            inDimensions (Dims 0 1 0 1) (1, 0) `shouldBe` True
        it "Below yMin is external" $
            inDimensions (Dims 0 1 0 1) (0, -0.1) `shouldBe` False
        it "At yMin is internal" $
            inDimensions (Dims 0 1 0 1) (0, 0) `shouldBe` True
        it "Above yMax is external" $
            inDimensions (Dims 0 1 0 1) (0, 1.1) `shouldBe` False
        it "At yMax is internal" $
            inDimensions (Dims 0 1 0 1) (0, 1) `shouldBe` True
        it "Inside the dimensions is internal" $
            inDimensions (Dims 0 1 0 1) (0.5, 0.5) `shouldBe` True
        it "On the boundary (xMin, yMin) is internal" $
            inDimensions (Dims 0 1 0 1) (0, 0) `shouldBe` True
        it "On the boundary (xMax, yMax) is internal" $
            inDimensions (Dims 0 1 0 1) (1, 1) `shouldBe` True
        it "On the boundary (xMin, yMax) is internal" $
            inDimensions (Dims 0 1 0 1) (0, 1) `shouldBe` True

    describe "groupFst" $ do
        it "On int list tuples" $
            groupFst [(0,1),(0,2),(1,2),(1,2)] `shouldBe` [[(0,1),(0,2)],[(1,2),(1,2)]]
        it "On single element list" $
            groupFst [(1, 'a')] `shouldBe` [[(1, 'a')]]
        it "On list with identical elements" $
            groupFst [(1, 'a'), (1, 'b'), (1, 'c')] `shouldBe` [[(1, 'a'), (1, 'b'), (1, 'c')]]
        it "On list with different elements" $
            groupFst [(1, 'a'), (2, 'b'), (1, 'c'), (3, 'd')] `shouldBe` [[(1, 'a'), (1, 'c')], [(2, 'b')], [(3, 'd')]]
        it "On list with mixed elements" $ 
            groupFst [(1, 'a'), (2, 'b'), (2, 'c'), (1, 'd'), (3, 'e')] `shouldBe` [[(1, 'a'), (1, 'd')], [(2, 'b'), (2, 'c')], [(3, 'e')]]
    
    describe "uniqueFstMaxSnd" $ do
        -- it "On empty list" $ do
        --     uniqueFstMaxSnd [] `shouldBe` []
        it "On single element list" $
            uniqueFstMaxSnd [(1, "a")] `shouldBe` [(1, "a")]
        it "On list with identical elements" $
            uniqueFstMaxSnd [(1, "apple"), (1, "banana"), (1, "cherry")] `shouldBe` [(1, "cherry")]
        it "On list with different elements" $
            uniqueFstMaxSnd [(1, "apple"), (2, "banana"), (1, "cherry"), (3, "date")] `shouldBe` [(1, "cherry"), (2, "banana"), (3, "date")]
        it "On list with mixed elements" $
            uniqueFstMaxSnd [(1, "apple"), (2, "banana"), (2, "cherry"), (1, "date"), (3, "fig")] `shouldBe` [(1, "date"), (2, "cherry"), (3, "fig")]
        it "On list with negative values" $
            uniqueFstMaxSnd [(1, -10), (2, -20), (1, -15), (3, -5)] `shouldBe` [(1, -10), (2, -20), (3, -5)]
        it "On list with zero values" $
            uniqueFstMaxSnd [(1, 0), (2, 0), (1, 0), (3, 0)] `shouldBe` [(1, 0), (2, 0), (3, 0)]
        it "On list with string values" $
            uniqueFstMaxSnd [("a", "apple"), ("b", "banana"), ("a", "cherry"), ("c", "date")] `shouldBe` [("a", "cherry"), ("b", "banana"), ("c", "date")]
        it "On list with mixed string and number values" $
            uniqueFstMaxSnd [("a", "apple"), ("b", "2"), ("a", "cherry"), ("c", "3")] `shouldBe` [("a", "cherry"), ("b", "2"), ("c", "3")]

    describe "scale" $ do
        it "With equal minVal and maxVal" $
            evaluate (scale 1 1 10 5) `shouldThrow` errorCall "scale: minVal and maxVal cannot be equal (division by zero)"
        it "With typical values" $
            scale 0 10 100 5 `shouldBe` 50
        it "With minVal and maxVal swapped" $
            scale 10 0 100 5 `shouldBe` 50
        it "With negative values" $
            scale (-10) 10 100 (-5) `shouldBe` 25
        it "With zero values" $
            scale 0 10 100 0 `shouldBe` 0
        it "With maxVal" $
            scale 0 10 100 10 `shouldBe` 100
        it "With minVal" $
            scale 0 10 100 0 `shouldBe` 0
        it "With values outside the range" $
            scale 0 10 100 15 `shouldBe` 150
        it "With fractional values" $
            scale 0 1 100 0.5 `shouldBe` 50
        it "With large values" $
            scale 0 1000000 100 500000 `shouldBe` 50