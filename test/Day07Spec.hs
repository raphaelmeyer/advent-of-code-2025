{-# LANGUAGE OverloadedStrings #-}

module Day07Spec where

import qualified AoC
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Day07
import Test.Hspec

exampleInput :: Text.Text
exampleInput =
  Text.intercalate
    "\n"
    [ ".......S.......",
      "...............",
      ".......^.......",
      "...............",
      "......^.^......",
      "...............",
      ".....^.^.^.....",
      "...............",
      "....^.^...^....",
      "...............",
      "...^.^...^.^...",
      "...............",
      "..^...^.....^..",
      "...............",
      ".^.^.^.^.^...^.",
      "..............."
    ]

spec :: Spec
spec = do
  let input = Day07.parse exampleInput

  describe "Parse input" $ do
    it "should parse the start position" $ do
      Day07.start input `shouldBe` 7

    it "should parse the manifolds" $ do
      length (Day07.manifolds input) `shouldBe` 15

    it "should parse the splitter positions" $ do
      let manifolds = Day07.manifolds input
      Set.size (manifolds !! 0) `shouldBe` 0
      Set.size (manifolds !! 11) `shouldBe` 3
      (manifolds !! 11) `shouldSatisfy` Set.member 2
      (manifolds !! 11) `shouldSatisfy` Set.member 6
      (manifolds !! 11) `shouldSatisfy` Set.member 12

  describe "Part one" $ do
    it "should solve example part one" $ do
      let solution = Day07.solve exampleInput
      AoC.one solution `shouldBe` "21"

    it "should count the splits" $ do
      Day07.countSplits input `shouldBe` 21
