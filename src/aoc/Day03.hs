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
findMaximum 0 _ = []
findMaximum n batteries = maxJoltage : findMaximum (n - 1) remaining
  where
    maxJoltage = maximum . validSearchRange $ batteries
    remaining = tail $ dropWhile (/= maxJoltage) batteries
    validSearchRange = take (length batteries - n + 1)
