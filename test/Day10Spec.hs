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
      AoC.two solution `shouldBe` "33"

  describe "Part two" $ do
    it "should subtract joltage for pressed button" $ do
      let button = [False, True, True, False, True]
          joltage = [2, 4, 6, 7, 12]
      Day10.unpressJoltageButton button joltage `shouldBe` [2, 3, 5, 7, 11]

    it "should count fewest presses for a joltage" $ do
      let fewestPresses = map Day10.countJoltageButtonPresses $ Day10.parse exampleInput
      fewestPresses `shouldBe` [10, 12, 11]

    it "should find button presses from all off to all off again" $ do
      let buttons = [[True, True, False], [True, False, True], [False, True, True]]
          combinations = Day10.findButtonsForIndicator [False, False, False] buttons [False, False, False]
      length combinations `shouldBe` 2
      combinations `shouldSatisfy` elem []
      combinations `shouldSatisfy` elem [[True, True, False], [True, False, True], [False, True, True]]

      let buttons2 = [[True, False, False], [True, True, True], [False, True, True]]
          combinations2 = Day10.findButtonsForIndicator [False, False, False] buttons2 [False, False, False]
      length combinations2 `shouldBe` 2
      combinations2 `shouldSatisfy` elem []
      combinations2 `shouldSatisfy` elem [[True, False, False], [True, True, True], [False, True, True]]

      let buttons3 = [[True, False, False]]
          combinations3 = Day10.findButtonsForIndicator [False, False, False] buttons3 [False, False, False]
      length combinations3 `shouldBe` 1
      combinations3 `shouldSatisfy` elem []

      let buttons4 = [[False, False, False]]
          combinations4 = Day10.findButtonsForIndicator [False, False, False] buttons4 [False, False, False]
      length combinations4 `shouldBe` 2
      combinations4 `shouldSatisfy` elem []
      combinations4 `shouldSatisfy` elem [[False, False, False]]

  describe "Parse input" $ do
    it "should parse all machines" $ do
      let machines = Day10.parse exampleInput
      length machines `shouldBe` 3

    describe "Parse machine" $ do
      let machine = Day10.parseMachine "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}"

      it "should parse indicator" $ do
        Day10.mIndicator machine `shouldBe` [False, False, False, True, False]

      it "should parse buttons" $ do
        let buttons = Day10.mButtons machine
        length buttons `shouldBe` 5
        buttons `shouldSatisfy` elem [True, False, True, True, True]
        buttons `shouldSatisfy` elem [False, False, True, True, False]
        buttons `shouldSatisfy` elem [True, False, False, False, True]
        buttons `shouldSatisfy` elem [True, True, True, False, False]
        buttons `shouldSatisfy` elem [False, True, True, True, True]

      it "should parse joltage" $ do
        Day10.mJoltage machine `shouldBe` [7, 5, 12, 7, 2]
