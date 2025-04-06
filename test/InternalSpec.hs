module InternalSpec (spec) where 

import Test.Hspec
import Data.Maybe
import Control.Exception (evaluate)

import Brick.Widgets.Plot.Internal


spec :: Spec
spec = do
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