{-# LANGUAGE OverloadedStrings #-}

module Day02 where

import qualified AoC
import Data.Int
import qualified Data.Text as Text

data Range = Range {from :: Int64, to :: Int64}
  deriving (Eq, Show)

solve :: Text.Text -> AoC.Solution
solve input = AoC.Solution (partOne ranges) (partTwo ranges)
  where
    ranges = parseInput input

partOne :: [Range] -> Text.Text
partOne = Text.pack . show . sum . concatMap (findInvalidIds isValidId)

partTwo :: [Range] -> Text.Text
partTwo = Text.pack . show . sum . concatMap (findInvalidIds isValidId2)

parseInput :: Text.Text -> [Range]
parseInput = map parseRange . Text.splitOn ","

parseRange :: Text.Text -> Range
parseRange input = case Text.splitOn "-" input of
  [f, t] -> Range (read . Text.unpack $ f) (read . Text.unpack $ t)
  _ -> undefined

findInvalidIds :: (Int64 -> Bool) -> Range -> [Int64]
findInvalidIds condition (Range f t) = filter condition [f .. t]

isValidId :: Int64 -> Bool
isValidId = uncurry (==) . splitInHalf . Text.pack . show

splitInHalf :: Text.Text -> (Text.Text, Text.Text)
splitInHalf string = Text.splitAt middle string
  where
    middle = (`div` 2) . Text.length $ string

isValidId2 :: Int64 -> Bool
isValidId2 n = any (isRepeated string . (`Text.take` string)) [1 .. half]
  where
    string = Text.pack . show $ n
    half = Text.length string `div` 2

isRepeated :: Text.Text -> Text.Text -> Bool
isRepeated string chunk = string == Text.replicate repetitions chunk
  where
    repetitions = Text.length string `div` Text.length chunk
