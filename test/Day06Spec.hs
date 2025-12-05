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
