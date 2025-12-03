{-# LANGUAGE OverloadedStrings #-}

module Day03Spec where

import qualified AoC
import qualified Data.Text as Text
import qualified Day03
import Test.Hspec

exampleInput :: Text.Text
exampleInput =
  Text.concat
    [ "987654321111111\n",
      "811111111111119\n",
      "234234234234278\n",
      "818181911112111\n"
    ]

spec :: Spec
spec = do
  let input = Day03.parseInput exampleInput

  describe "Parse input" $ do
    it "split lines" $ do
      length input `shouldBe` 4
      input !! 2 `shouldBe` "234234234234278"

  describe "Part one" $ do
    it "should solve part one" $ do
      let solution = Day03.solve exampleInput
      AoC.one solution `shouldBe` "357"
