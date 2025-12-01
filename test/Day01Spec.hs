{-# LANGUAGE OverloadedStrings #-}

module Day01Spec where

import qualified AoC
import qualified Data.Text as Text
import qualified Day01
import Test.Hspec

example1 :: Text.Text
example1 =
  Text.concat
    [ "L68\n",
      "L30\n",
      "R48\n",
      "L5\n",
      "R60\n",
      "L55\n",
      "L1\n",
      "L99\n",
      "R14\n",
      "L82\n"
    ]

spec :: Spec
spec = do
  describe "Secret Entrance" $ do
    it "solve part one" $ do
      let solution = Day01.solve example1
      AoC.one solution `shouldBe` "3"

    it "solve part two" $ do
      let solution = Day01.solve example1
      AoC.two solution `shouldBe` "6"

    it "parse input" $ do
      let input = Day01.parse example1
      length input `shouldBe` 10
      input !! 3 `shouldBe` Day01.RotLeft 5
      input !! 8 `shouldBe` Day01.RotRight 14

    describe "Part one" $ do
      it "rotate" $ do
        Day01.rot 50 (Day01.RotLeft 23) `shouldBe` 27
        Day01.rot 50 (Day01.RotRight 23) `shouldBe` 73
        Day01.rot 0 (Day01.RotLeft 1) `shouldBe` 99
        Day01.rot 99 (Day01.RotRight 1) `shouldBe` 0

    describe "Part two" $ do
      it "no rotation" $ do
        Day01.countPassingZero [] `shouldBe` 0

      it "rotate not passing zero" $ do
        Day01.countPassingZero [Day01.RotLeft 13] `shouldBe` 0
        Day01.countPassingZero [Day01.RotRight 13] `shouldBe` 0

      it "rotate past zero" $ do
        Day01.countPassingZero [Day01.RotLeft 77] `shouldBe` 1
        Day01.countPassingZero [Day01.RotRight 77] `shouldBe` 1

      it "rotate to zero" $ do
        Day01.countPassingZero [Day01.RotLeft 50] `shouldBe` 1
        Day01.countPassingZero [Day01.RotRight 50] `shouldBe` 1

      it "rotate past zero, then to zero again" $ do
        Day01.countPassingZero [Day01.RotLeft 150] `shouldBe` 2
        Day01.countPassingZero [Day01.RotRight 150] `shouldBe` 2

      it "rotate from zero" $ do
        Day01.countPassingZero [Day01.RotLeft 50, Day01.RotLeft 50] `shouldBe` 1
        Day01.countPassingZero [Day01.RotRight 50, Day01.RotRight 50] `shouldBe` 1

      it "rotate to zero and back again" $ do
        Day01.countPassingZero [Day01.RotLeft 50, Day01.RotRight 50] `shouldBe` 1
        Day01.countPassingZero [Day01.RotRight 50, Day01.RotLeft 50] `shouldBe` 1

      it "rotate from zero to zero again" $ do
        Day01.countPassingZero [Day01.RotLeft 50, Day01.RotLeft 100] `shouldBe` 2
        Day01.countPassingZero [Day01.RotRight 50, Day01.RotRight 100] `shouldBe` 2

      it "rotate from zero once passing zero" $ do
        Day01.countPassingZero [Day01.RotLeft 50, Day01.RotLeft 123] `shouldBe` 2
        Day01.countPassingZero [Day01.RotRight 50, Day01.RotRight 123] `shouldBe` 2
