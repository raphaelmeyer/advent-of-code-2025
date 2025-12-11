{-# LANGUAGE OverloadedStrings #-}

module Day11Spec where

import qualified AoC
import qualified Data.Text as Text
import qualified Day11
import Test.Hspec

exampleInput :: Text.Text
exampleInput =
  Text.intercalate
    "\n"
    [ "aaa: you hhh",
      "you: bbb ccc",
      "bbb: ddd eee",
      "ccc: ddd eee fff",
      "ddd: ggg",
      "eee: out",
      "fff: out",
      "ggg: out",
      "hhh: ccc fff iii",
      "iii: out"
    ]

spec :: Spec
spec = do
  describe "Example" $ do
    it "should solve example part one" $ do
      let solution = Day11.solve exampleInput
      AoC.one solution `shouldBe` "5"
