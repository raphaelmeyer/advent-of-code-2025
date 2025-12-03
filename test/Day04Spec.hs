{-# LANGUAGE OverloadedStrings #-}

module Day04Spec where

import qualified AoC
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Day04
import Test.Hspec

exampleInput :: Text.Text
exampleInput =
  Text.intercalate
    "\n"
    [ "..@@.@@@@.",
      "@@@.@.@.@@",
      "@@@@@.@.@@",
      "@.@@@@..@.",
      "@@.@@@@.@@",
      ".@@@@@@@.@",
      ".@.@.@.@@@",
      "@.@@@.@@@@",
      ".@@@@@@@@.",
      "@.@.@@@.@."
    ]

spec :: Spec
spec = do
  let input = Day04.parse exampleInput
      solution = Day04.solve exampleInput

  describe "Parse input" $ do
    it "should parse the grid" $ do
      input `shouldNotSatisfy` Set.member (Day04.Position 0 0)
      input `shouldSatisfy` Set.member (Day04.Position 2 0)

      input `shouldNotSatisfy` Set.member (Day04.Position 5 1)
      input `shouldSatisfy` Set.member (Day04.Position 1 5)

  describe "Part one" $ do
    it "should solve part one" $ do
      AoC.one solution `shouldBe` "13"

    it "should count its neighbors" $ do
      Day04.countNeighbors input (Day04.Position 8 3) `shouldBe` 4
      Day04.countNeighbors input (Day04.Position 4 4) `shouldBe` 8
