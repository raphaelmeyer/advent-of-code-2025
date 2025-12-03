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
      solution = Day03.solve exampleInput

  describe "Parse input" $ do
    it "split lines" $ do
      length input `shouldBe` 4
      input !! 2 `shouldBe` "234234234234278"

  describe "Part one" $ do
    it "should solve part one" $ do
      AoC.one solution `shouldBe` "357"

    it "should find the maximum power of a bank" $ do
      Day03.findMaximum 2 "987654321111111" `shouldBe` "98"
      Day03.findMaximum 2 "811111111111119" `shouldBe` "89"
      Day03.findMaximum 2 "234234234234278" `shouldBe` "78"
      Day03.findMaximum 2 "818181911112111" `shouldBe` "92"

  describe "Part two" $ do
    it "should solve part two" $ do
      AoC.two solution `shouldBe` "3121910778619"

    it "should find the maximum power of a bank" $ do
      Day03.findMaximum 12 "987654321111111" `shouldBe` "987654321111"
      Day03.findMaximum 12 "811111111111119" `shouldBe` "811111111119"
      Day03.findMaximum 12 "234234234234278" `shouldBe` "434234234278"
      Day03.findMaximum 12 "818181911112111" `shouldBe` "888911112111"
