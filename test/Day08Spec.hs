{-# LANGUAGE OverloadedStrings #-}

module Day08Spec where

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Day08
import Test.Hspec

exampleInput :: Text.Text
exampleInput =
  Text.intercalate
    "\n"
    [ "162,817,812",
      "57,618,57",
      "906,360,560",
      "592,479,940",
      "352,342,300",
      "466,668,158",
      "542,29,236",
      "431,825,988",
      "739,650,466",
      "52,470,668",
      "216,146,977",
      "819,987,18",
      "117,168,530",
      "805,96,715",
      "346,949,466",
      "970,615,88",
      "941,993,340",
      "862,61,35",
      "984,92,344",
      "425,690,689"
    ]

spec :: Spec
spec = do
  let boxes = Day08.parse exampleInput

  describe "Parse input" $ do
    it "should parse junction box positions" $ do
      Set.size boxes `shouldBe` 20
      boxes `shouldSatisfy` Set.member (Day08.Position 57 618 57)
      boxes `shouldSatisfy` Set.member (Day08.Position 431 825 988)

  describe "Part one" $ do
    it "should solve example part one" $ do
      let solution = Day08.largestCircuits 10 boxes
      solution `shouldBe` 40

    it "should find all unique pairs" $ do
      let pairs = Day08.uniquePairs boxes
      Set.size pairs `shouldBe` 190 -- (n - 1)(n)/2
      pairs `shouldSatisfy` Set.member (Day08.Pair (Day08.Position 52 470 668) (Day08.Position 57 618 57))
      pairs `shouldNotSatisfy` Set.member (Day08.Pair (Day08.Position 57 618 57) (Day08.Position 52 470 668))
