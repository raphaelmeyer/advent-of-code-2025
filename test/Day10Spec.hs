{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}

module Day10Spec where

import qualified AoC
import qualified Data.Text as Text
import qualified Day10
import Test.Hspec

exampleInput :: Text.Text
exampleInput =
  Text.intercalate
    "\n"
    [ "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}",
      "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}",
      "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"
    ]

spec :: Spec
spec = do
  describe "Example solution" $ do
    it "should solve the example" $ do
      let solution = Day10.solve exampleInput
      AoC.one solution `shouldBe` "7"

  describe "Parse input" $ do
    it "should parse all machines" $ do
      let machines = Day10.parse exampleInput
      length machines `shouldBe` 3

    it "should parse a machine" $ do
      let machine = Day10.parseMachine "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}"
      Day10.mIndicator machine `shouldBe` 0b01000

      let buttons = Day10.mButtons machine
      length buttons `shouldBe` 5
      buttons `shouldSatisfy` elem 0b11101 -- (0,2,3,4)
      buttons `shouldSatisfy` elem 0b01100 -- (2,3)
      buttons `shouldSatisfy` elem 0b10001 -- (0,4)
      buttons `shouldSatisfy` elem 0b00111 -- (0,1,2)
      buttons `shouldSatisfy` elem 0b11110 -- (1,2,3,4)
