{-# LANGUAGE OverloadedStrings #-}

module Day09Spec where

import qualified AoC
import qualified Data.Text as Text
import qualified Day09
import Test.Hspec

exampleInput :: Text.Text
exampleInput =
  Text.intercalate
    "\n"
    [ "7,1",
      "11,1",
      "11,7",
      "9,7",
      "9,5",
      "2,5",
      "2,3",
      "7,3"
    ]

spec :: Spec
spec = do
  let input = Day09.parse exampleInput

  describe "Parse input" $ do
    it "should build all rectangles" $ do
      let rectangles = Day09.allRectangles input
      length rectangles `shouldBe` 28

  describe "Part one" $ do
    it "should solve example part one" $ do
      let solution = Day09.solve exampleInput
      AoC.one solution `shouldBe` "50"
