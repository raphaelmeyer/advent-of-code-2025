{-# LANGUAGE OverloadedStrings #-}

module Day06Spec where

import qualified AoC
import qualified Data.Text as Text
import qualified Day06
import Test.Hspec

exampleInput :: Text.Text
exampleInput =
  Text.intercalate
    "\n"
    [ "123 328  51 64 ",
      " 45 64  387 23 ",
      "  6 98  215 314",
      "*   +   *   +  "
    ]

spec :: Spec
spec = do
  describe "Part one" $ do
    it "should solve part one" $ do
      let solution = Day06.solve exampleInput
      AoC.one solution `shouldBe` "4277556"

  describe "Part two" $ do
    it "should solve part two" $ do
      let solution = Day06.solve exampleInput
      AoC.two solution `shouldBe` "3263827"

    describe "Parse cephalopod math" $ do
      let (numbers, ops) = Day06.cephaloParse exampleInput

      it "should parse number of problems" $ do
        length numbers `shouldBe` 4
        length ops `shouldBe` 4

      it "should parse vertical numbers" $ do
        numbers !! 0 `shouldMatchList` [1, 24, 356]
        numbers !! 1 `shouldMatchList` [369, 248, 8]
        numbers !! 3 `shouldMatchList` [4, 431, 623]

      it "should parse the operation" $ do
        ops !! 0 `shouldBe` Day06.Mult
        ops !! 2 `shouldBe` Day06.Mult
        ops !! 3 `shouldBe` Day06.Add
