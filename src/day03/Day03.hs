{-# LANGUAGE OverloadedStrings #-}

module Day03 where

import qualified AoC
import Data.Int
import qualified Data.Text as Text

solve :: Text.Text -> AoC.Solution
solve input = AoC.Solution (partOne banks) (partTwo banks)
  where
    banks = parseInput input

parseInput :: Text.Text -> [Text.Text]
parseInput = Text.lines

partOne :: [Text.Text] -> Text.Text
partOne = sumJoltage 2

partTwo :: [Text.Text] -> Text.Text
partTwo = sumJoltage 12

sumJoltage :: Int -> [Text.Text] -> Text.Text
sumJoltage n = Text.pack . show . sum . map (asNumber . findMaximum n . Text.unpack)
  where
    asNumber = read :: String -> Int64

findMaximum :: Int -> String -> String
findMaximum 0 _ = undefined
findMaximum 1 bank = [maximum bank]
findMaximum n bank = searchMaximum n ('0', []) bank

searchMaximum :: Int -> (Char, String) -> String -> String
searchMaximum _ _ [] = undefined
searchMaximum n (current, on) (j : js)
  | length (j : js) < n = current : on
  | otherwise = searchMaximum n updated js
  where
    updated = if current < j then (j, on') else (current, on)
    on' = findMaximum (n - 1) js
