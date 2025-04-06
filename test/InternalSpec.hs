module InternalSpec (spec) where 

import Test.Hspec
import Data.Maybe
import Control.Exception (evaluate)

import Brick.Widgets.Plot.Internal


spec :: Spec
spec = do
    -- describe "scale" $ do
    --     it "returns a negative value when outside of range" $ do
    --         scale 0.0 1.0 2.0 5 `shouldBe` (-5)

    --     it "returns the minimum value when given the minimum" $ do
    --         scale 1.0 1.0 2.0 5 `shouldBe` 0 

    --     it "returns the maximum value when given the maximum" $ do
    --         scale 2.0 1.0 2.0 5 `shouldBe` 5 

    --     it "returns a value in the middle when given the middle" $ do
    --         scale 1.0 0.0 2.0 6 `shouldBe` 3 

    --     it "returns a value rounded to the nearest integer" $ do
    --         scale 1.0 0 2.0 5 `shouldBe` 2

    --     it "returns the same value when input is equal to the midpoint" $ do
    --         scale 1.0 0 2.0 4 `shouldBe` 2

    --     it "returns the minimum value when input is less than the minimum" $ do
    --         scale 0 1.0 2.0 5 `shouldBe` (-5)

    --     it "returns the maximum value when input is greater than the maximum" $ do
    --         scale 3.0 1.0 2.0 5 `shouldBe` 10

    --     it "handles negative scaling correctly" $ do
    --         scale 1.0 2.0 0.0 4 `shouldBe` 2

    --     it "returns a value when the range is negative" $ do
    --         scale 1.0 2.0 0.0 (-4) `shouldBe` (-2)

    --     it "returns an error when the range is zero" $ do
    --         evaluate (scale 1.0 1.0 1.0 5) `shouldThrow` errorCall "scale: minVal and maxVal cannot be equal (division by zero)"

        describe "validDimensions" $ do
            it "xMin and xMax equal is not valid" $ do
                validDimensions (Dims 1 1 0 1) `shouldBe` False
            it "yMin and yMax equal is not valid" $ do
                validDimensions (Dims 0 1 1 1) `shouldBe` False
            it "xMin greater than xMax equal is not valid" $ do
                validDimensions (Dims 1 0 1 2) `shouldBe` False
            it "yMin greater than yMax equal is not valid" $ do
                validDimensions (Dims 2 4 4 2) `shouldBe` False
            it "Small dimensions are valid" $ do
                validDimensions (Dims 0 0.1 0 0.1) `shouldBe` True
            it "Large dimensions are valid" $ do
                validDimensions (Dims 10 1000 10 1000) `shouldBe` True

        describe "inDimensions" $ do
            it "Below xMin is external" $ do
                inDimensions (Dims 0 1 0 1) (-0.1, 0) `shouldBe` False
            it "At xMin is internal" $ do
                inDimensions (Dims 0 1 0 1) (0, 0) `shouldBe` True
            it "Above xMax is external" $ do
                inDimensions (Dims 0 1 0 1) (1.1, 0) `shouldBe` False
            it "At xMax is internal" $ do
                inDimensions (Dims 0 1 0 1) (1, 0) `shouldBe` True
            it "Below yMin is external" $ do
                inDimensions (Dims 0 1 0 1) (0, -0.1) `shouldBe` False
            it "At yMin is internal" $ do
                inDimensions (Dims 0 1 0 1) (0, 0) `shouldBe` True
            it "Above yMax is external" $ do
                inDimensions (Dims 0 1 0 1) (0, 1.1) `shouldBe` False
            it "At yMax is internal" $ do
                inDimensions (Dims 0 1 0 1) (0, 1) `shouldBe` True
            it "Inside the dimensions is internal" $ do
                inDimensions (Dims 0 1 0 1) (0.5, 0.5) `shouldBe` True
            it "On the boundary (xMin, yMin) is internal" $ do
                inDimensions (Dims 0 1 0 1) (0, 0) `shouldBe` True
            it "On the boundary (xMax, yMax) is internal" $ do
                inDimensions (Dims 0 1 0 1) (1, 1) `shouldBe` True
            it "On the boundary (xMin, yMax) is internal" $ do
                inDimensions (Dims 0 1 0 1) (0, 1) `shouldBe` True

        describe "groupFst" $ do
            it "groupFst on int list tuples" $ do
                groupFst [(0,1),(0,2),(1,2),(1,2)] `shouldBe` [[(0,1),(0,2)],[(1,2),(1,2)]]
            
    -- describe "toIndex" $ do
    --     let c = Context 0 100 0 100 10 10
    --     it "Bottom left" $ do
    --         toIndex c (0, 0) `shouldBe` Just 90

    --     it "Bottom right" $ do
    --         toIndex c (100, 0) `shouldBe` Just 99

    --     it "Center" $ do
    --         toIndex c (50, 50) `shouldBe` Just 44

    --     it "Top left" $ do
    --         toIndex c (0, 100) `shouldBe` Just 0

    --     it "Top right" $ do
    --         toIndex c (100, 100) `shouldBe` Just 9

    --     it "Outside bottom left" $ do
    --         toIndex c (-10, -10) `shouldBe` Nothing

    --     it "Outside bottom right" $ do
    --         toIndex c (110, -10) `shouldBe` Nothing

    --     it "Outside top left" $ do
    --         toIndex c (-10, 110) `shouldBe` Nothing

    --     it "Outside top right" $ do
    --         toIndex c (110, 110) `shouldBe` Nothing

    --     it "Negative x value" $ do
    --         toIndex c (-5, 50) `shouldBe` Nothing

    --     it "Negative y value" $ do
    --         toIndex c (50, -5) `shouldBe` Nothing

    --     it "Edges" $ do
    --         toIndex  c (0, 50) `shouldBe` Just 40
    --         toIndex  c (100, 50) `shouldBe` Just 49
    --         toIndex c (50, 0) `shouldBe` Just 94
    --         toIndex c (50, 100) `shouldBe` Just 4

    --     it "Very small canvas" $ do
    --         let smallCanvas = Context 0 1 0 1 1 1
    --         toIndex smallCanvas (0, 0) `shouldBe` Just 0
    --         toIndex smallCanvas (1, 1) `shouldBe` Just 0
    --         toIndex smallCanvas (0.5, 0.5) `shouldBe` Just 0

    -- describe "scatterCanvas" $ do
    --     let canvas = Context 0 100 0 100 10 10
    