{-# LANGUAGE OverloadedStrings #-}

module Day06 where

import qualified AoC
import Data.Int
import qualified Data.List as List
import qualified Data.Text as Text

data Op = Add | Mult

solve :: Text.Text -> AoC.Solution
solve input = AoC.Solution (partOne problems) "The universe and everything"
  where
    problems = parse input

partOne :: ([[Int64]], [Op]) -> Text.Text
partOne = Text.pack . show . uncurry solveMath

-- parse

parse :: Text.Text -> ([[Int64]], [Op])
parse input = (map parseNumbers . init $ rows, parseOps . last $ rows)
  where
    rows = Text.lines input

parseNumbers :: Text.Text -> [Int64]
parseNumbers = map (read . Text.unpack) . filter (not . Text.null) . Text.split (== ' ')

parseOps :: Text.Text -> [Op]
parseOps = map (op . Text.head) . filter (not . Text.null) . Text.split (== ' ')
  where
    op '+' = Add
    op '*' = Mult
    op _ = undefined

-- part one

solveMath :: [[Int64]] -> [Op] -> Int64
solveMath numbers ops = sum . zipWith calc ops $ List.transpose numbers

calc :: Op -> [Int64] -> Int64
calc Add = sum
calc Mult = product
