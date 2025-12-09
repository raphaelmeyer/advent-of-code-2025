{-# LANGUAGE OverloadedStrings #-}

module Day09 where

import qualified AoC
import qualified Data.List as List
import qualified Data.Text as Text

data Corner = Corner Int Int

data Rectangle = Rectangle Corner Corner

solve :: Text.Text -> AoC.Solution
solve input = AoC.Solution (partOne rectangles) "The universe and everything"
  where
    corners = parse input
    rectangles = allRectangles corners

partOne :: [Rectangle] -> Text.Text
partOne = Text.pack . show . largestArea

-- parse input

parse :: Text.Text -> [Corner]
parse = map parseCorner . Text.lines

parseCorner :: Text.Text -> Corner
parseCorner input = case coords input of
  [x, y] -> Corner x y
  _ -> undefined
  where
    coords = map (read . Text.unpack) . Text.splitOn ","

allRectangles :: [Corner] -> [Rectangle]
allRectangles corners = [Rectangle a b | (a : rest) <- List.tails corners, b <- rest]

-- part one

largestArea :: [Rectangle] -> Int
largestArea = maximum . map rectangleArea

rectangleArea :: Rectangle -> Int
rectangleArea (Rectangle (Corner ax ay) (Corner bx by)) = (abs (ax - bx) + 1) * (abs (ay - by) + 1)
