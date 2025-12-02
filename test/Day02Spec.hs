{-# LANGUAGE OverloadedStrings #-}

module Day02Spec where

import qualified AoC
import qualified Data.Text as Text
import qualified Day02
import Test.Hspec

exampleInput :: Text.Text
exampleInput = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"

spec :: Spec
spec = do
  let input = Day02.parseInput exampleInput

  describe "Parse input" $ do
    it "should parse input" $ do
      length input `shouldBe` 11
      input !! 0 `shouldBe` Day02.Range 11 22
      input !! 7 `shouldBe` Day02.Range 38593856 38593862

  describe "Part one" $ do
    it "should solve example input" $ do
      let solution = Day02.solve exampleInput
      AoC.one solution `shouldBe` "1227775554"

    it "should identify invalid ID" $ do
      11 `shouldSatisfy` Day02.isValidId
      123123 `shouldSatisfy` Day02.isValidId
      101 `shouldNotSatisfy` Day02.isValidId
      1111 `shouldSatisfy` Day02.isValidId
      1112 `shouldNotSatisfy` Day02.isValidId
      11111 `shouldNotSatisfy` Day02.isValidId
      9876598765 `shouldSatisfy` Day02.isValidId

    it "should find invalid IDs in range" $ do
      Day02.findInvalidIds (Day02.Range 11 22) `shouldMatchList` [11, 22]

    it "should find all invalid IDs" $ do
      Day02.findAllInvalidIds input `shouldMatchList` [11, 22, 99, 1010, 1188511885, 222222, 446446, 38593859]
