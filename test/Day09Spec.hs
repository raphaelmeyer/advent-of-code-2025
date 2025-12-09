{-# LANGUAGE OverloadedStrings #-}

module Day09Spec where

import qualified AoC
import qualified Data.Text as Text
import qualified Day09
import Test.Hspec

exampleInput :: Text.Text
exampleInput =
  Text.intercalate
    "\n"
    [ "7,1",
      "11,1",
      "11,7",
      "9,7",
      "9,5",
      "2,5",
      "2,3",
      "7,3"
    ]

example2 :: Text.Text
example2 =
  Text.intercalate
    "\n"
    [ "1,3",
      "1,7",
      "20,7",
      "20,1",
      "8,1",
      "8,5",
      "2,5",
      "2,3"
    ]

--
--         #-----------------------#
--         |                       |
--      #--#                       |
--      |                          #--#
--  #---#                             |
--  |                                 #--#
--  #----------------------------#       |
--                               |       |
--   #---------------------------#       |
--   |                                   |
--   #-------#                 #---------#
--           #-----------------#
--
-- 12,39 - 34,45 -> 161

example3 :: Text.Text
example3 =
  Text.intercalate
    "\n"
    [ "12,39",
      "12,41",
      "9,41",
      "9,43",
      "5,43",
      "5,45",
      "34,45",
      "34,47",
      "6,47",
      "6,49",
      "14,49",
      "14,50",
      "32,50",
      "32,49",
      "42,49",
      "42,44",
      "39,44",
      "39,42",
      "36,42",
      "36,39"
    ]

spec :: Spec
spec = do
  let input = Day09.parse exampleInput

  describe "Parse input" $ do
    it "should build all rectangles" $ do
      let rectangles = Day09.allRectangles input
      length rectangles `shouldBe` 28

  describe "Part one" $ do
    it "should solve example part one" $ do
      let solution = Day09.solve exampleInput
      AoC.one solution `shouldBe` "50"

  describe "Part two" $ do
    it "should solve example part two" $ do
      let solution = Day09.solve exampleInput
      AoC.two solution `shouldBe` "24"

    it "should solve part two example 2" $ do
      let solution = Day09.solve example2
      AoC.two solution `shouldBe` "91"

    it "should solve part two example 3" $ do
      let solution = Day09.solve example3
      AoC.two solution `shouldBe` "161"

    it "should find all vertical lines" $ do
      let verticals = Day09.allVerticals input
      length verticals `shouldBe` 4
      verticals `shouldSatisfy` elem (Day09.Vertical 2 3 5)
      verticals `shouldSatisfy` elem (Day09.Vertical 11 1 7)

      let verticals3 = Day09.allVerticals . Day09.parse $ example3
      length verticals3 `shouldBe` 10
      verticals3 `shouldSatisfy` elem (Day09.Vertical 6 47 49)
      verticals3 `shouldSatisfy` elem (Day09.Vertical 39 42 44)

    it "should normalize rectangles" $ do
      let normalized = Day09.normalizeRectangles . Day09.allRectangles $ input
      normalized `shouldSatisfy` elem (Day09.Rectangle (Day09.Corner 2 1) (Day09.Corner 11 3))
      normalized `shouldNotSatisfy` elem (Day09.Rectangle (Day09.Corner 2 3) (Day09.Corner 11 1))
      normalized `shouldNotSatisfy` elem (Day09.Rectangle (Day09.Corner 11 1) (Day09.Corner 2 3))
      normalized `shouldNotSatisfy` elem (Day09.Rectangle (Day09.Corner 11 3) (Day09.Corner 2 1))

    it "should sort rectangles by area" $ do
      let rectangles = Day09.sortedByArea . Day09.normalizeRectangles . Day09.allRectangles $ input
      (fst . head $ rectangles) `shouldBe` 50
      (fst . last $ rectangles) `shouldBe` 3

    it "should check if rectangle is inside" $ do
      let corners = Day09.parse example3
          verticals = Day09.allVerticals corners
          horizontals = Day09.allHorizontals corners
      Day09.Rectangle (Day09.Corner 12 39) (Day09.Corner 34 45) `shouldSatisfy` Day09.isInside verticals horizontals
      Day09.Rectangle (Day09.Corner 6 47) (Day09.Corner 32 49) `shouldSatisfy` Day09.isInside verticals horizontals
      Day09.Rectangle (Day09.Corner 34 39) (Day09.Corner 36 45) `shouldSatisfy` Day09.isInside verticals horizontals

    it "should check if rectangle is not inside" $ do
      let corners = Day09.parse example3
          verticals = Day09.allVerticals corners
          horizontals = Day09.allHorizontals corners
      Day09.Rectangle (Day09.Corner 9 42) (Day09.Corner 32 49) `shouldNotSatisfy` Day09.isInside verticals horizontals
      Day09.Rectangle (Day09.Corner 32 42) (Day09.Corner 36 49) `shouldNotSatisfy` Day09.isInside verticals horizontals
      Day09.Rectangle (Day09.Corner 9 39) (Day09.Corner 12 43) `shouldNotSatisfy` Day09.isInside verticals horizontals
      Day09.Rectangle (Day09.Corner 6 45) (Day09.Corner 34 47) `shouldNotSatisfy` Day09.isInside verticals horizontals
