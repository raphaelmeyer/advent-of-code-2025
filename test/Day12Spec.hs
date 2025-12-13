{-# LANGUAGE OverloadedStrings #-}

module Day12Spec where

import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Day12
import Test.Hspec

exampleInput :: Text.Text
exampleInput =
  Text.intercalate
    "\n"
    [ "0:",
      "###",
      "##.",
      "##.",
      "",
      "1:",
      "###",
      "##.",
      ".##",
      "",
      "2:",
      ".##",
      "###",
      "##.",
      "",
      "3:",
      "##.",
      "###",
      "##.",
      "",
      "4:",
      "###",
      "#..",
      "###",
      "",
      "5:",
      "###",
      ".#.",
      "###",
      "",
      "4x4: 0 0 0 0 2 0",
      "12x5: 1 0 1 0 2 2",
      "12x5: 1 0 1 0 3 2"
    ]

spec :: Spec
spec = do
  describe "Parse input" $ do
    it "should parse the input" $ do
      let (shapes, trees) = Day12.parse exampleInput
      Map.size shapes `shouldBe` 6
      length trees `shouldBe` 3

  describe "Counting stuff" $ do
    let (shapes, trees) = Day12.parse exampleInput
    it "should count presents under tree" $ do
      map Day12.sumPresents trees `shouldBe` [2, 6, 7]

    it "should count 3x3 squares under tree" $ do
      map Day12.count3x3Squares trees `shouldBe` [1, 4, 4]

    it "should calculate shape area" $ do
      Day12.shapeArea (shapes Map.! 0) `shouldBe` 7
      Day12.shapeArea (shapes Map.! 2) `shouldBe` 7
      Day12.shapeArea (shapes Map.! 5) `shouldBe` 7

    it "should calculate theoretical minimal required area" $ do
      map (Day12.minRequiredArea shapes . Day12.treePresents) trees `shouldBe` [14, 42, 49]

    it "should calculate area under tree" $ do
      map (uncurry (*) . Day12.treeArea) trees `shouldBe` [16, 60, 60]
