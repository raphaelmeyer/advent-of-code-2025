{-# LANGUAGE OverloadedStrings #-}

module Day05Spec where

import qualified AoC
import qualified Data.Text as Text
import qualified Day05
import Test.Hspec

exampleInput :: Text.Text
exampleInput =
  Text.intercalate
    "\n"
    [ "3-5",
      "10-14",
      "16-20",
      "12-18",
      "",
      "1",
      "5",
      "8",
      "11",
      "17",
      "32"
    ]

spec :: Spec
spec = do
  let input = Day05.parse exampleInput

  describe "Parse input" $ do
    it "should parse fresh ingredients ranges" $ do
      length (Day05.dbFresh input) `shouldBe` 4
      Day05.dbFresh input `shouldContain` [Day05.Range 3 5]
      Day05.dbFresh input `shouldContain` [Day05.Range 12 18]

    it "should parse the ingredients list" $ do
      length (Day05.dbIngredients input) `shouldBe` 6
      Day05.dbIngredients input `shouldContain` [1]
      Day05.dbIngredients input `shouldContain` [17]
      Day05.dbIngredients input `shouldContain` [32]

  describe "Part one" $ do
    it "should solve part one" $ do
      let solution = Day05.solve exampleInput
      AoC.one solution `shouldBe` "3"

  describe "Part two" $ do
    it "should solve part two" $ do
      let solution = Day05.solve exampleInput
      AoC.two solution `shouldBe` "14"

    it "should merge overlapping intervals" $ do
      let ranges = Day05.combineRanges (Day05.dbFresh input)
      ranges `shouldMatchList` [Day05.Range 3 5, Day05.Range 10 20]
