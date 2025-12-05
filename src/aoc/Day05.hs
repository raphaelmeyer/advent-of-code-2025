{-# LANGUAGE OverloadedStrings #-}

module Day05 where

import qualified AoC
import Data.Int
import qualified Data.List as List
import qualified Data.Text as Text

type ID = Int64

data Range = Range ID ID
  deriving (Eq, Ord, Show)

data DataBase = DataBase {dbFresh :: [Range], dbIngredients :: [ID]}
  deriving (Eq, Show)

solve :: Text.Text -> AoC.Solution
solve input = AoC.Solution (partOne db) (partTwo db)
  where
    db = parse input

partOne :: DataBase -> Text.Text
partOne = Text.pack . show . countFreshIngredients

partTwo :: DataBase -> Text.Text
partTwo = Text.pack . show . sumRanges . combineRanges . dbFresh

-- parse input

parse :: Text.Text -> DataBase
parse input = DataBase fresh ingredients
  where
    fresh = parseRanges ranges
    ingredients = parseIds ids
    (ranges, ids) = break Text.null . Text.lines $ input

parseRanges :: [Text.Text] -> [Range]
parseRanges = map parseRange . filter (not . Text.null)

parseRange :: Text.Text -> Range
parseRange range = case Text.splitOn "-" range of
  [from, to] -> Range (read . Text.unpack $ from) (read . Text.unpack $ to)
  _ -> undefined

parseIds :: [Text.Text] -> [Int64]
parseIds = map (read . Text.unpack) . filter (not . Text.null)

-- part one

countFreshIngredients :: DataBase -> Int
countFreshIngredients = length . findFreshIngredients

findFreshIngredients :: DataBase -> [ID]
findFreshIngredients db = filter (`inAnyRange` (dbFresh db)) (dbIngredients db)

inAnyRange :: ID -> [Range] -> Bool
inAnyRange ingredient = any (ingredient `inRange`)

inRange :: ID -> Range -> Bool
inRange ingredient (Range from to) = from <= ingredient && ingredient <= to

-- part two

combineRanges :: [Range] -> [Range]
combineRanges = foldl mergeRange [] . List.sort

mergeRange :: [Range] -> Range -> [Range]
mergeRange [] range = [range]
mergeRange (Range currentFrom currentTo : merged) (Range from to)
  | currentTo < from = Range from to : Range currentFrom currentTo : merged
  | currentTo < to = Range currentFrom to : merged
  | otherwise = Range currentFrom currentTo : merged

sumRanges :: [Range] -> Int64
sumRanges = sum . map (\(Range from to) -> to - from + 1)
