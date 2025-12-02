{-# LANGUAGE OverloadedStrings #-}

module Day02 where

import qualified AoC
import Data.Int
import qualified Data.Text as Text

data Range = Range {from :: Int64, to :: Int64}
  deriving (Eq, Show)

solve :: Text.Text -> AoC.Solution
solve input = AoC.Solution (partOne ranges) "The universe and everything"
  where
    ranges = parseInput input

partOne :: [Range] -> Text.Text
partOne = Text.pack . show . sum . findAllInvalidIds

parseInput :: Text.Text -> [Range]
parseInput = map parseRange . Text.splitOn ","

parseRange :: Text.Text -> Range
parseRange input = case Text.splitOn "-" input of
  [f, t] -> Range (read . Text.unpack $ f) (read . Text.unpack $ t)
  _ -> undefined

isValidId :: Int64 -> Bool
isValidId = uncurry (==) . splitInHalf . Text.pack . show

splitInHalf :: Text.Text -> (Text.Text, Text.Text)
splitInHalf string = Text.splitAt middle string
  where
    middle = (`div` 2) . Text.length $ string

findInvalidIds :: Range -> [Int64]
findInvalidIds (Range f t) = filter isValidId [f .. t]

findAllInvalidIds :: [Range] -> [Int64]
findAllInvalidIds = concatMap findInvalidIds
