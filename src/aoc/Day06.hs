{-# LANGUAGE OverloadedStrings #-}

module Day06 where

import qualified AoC
import Data.Int
import qualified Data.List as List
import qualified Data.List.Extra as Extra
import qualified Data.Text as Text

data Op = Add | Mult deriving (Eq, Show)

solve :: Text.Text -> AoC.Solution
solve input = AoC.Solution (partOne input) (partTwo input)

partOne :: Text.Text -> Text.Text
partOne = Text.pack . show . uncurry solveMath . humanParse

partTwo :: Text.Text -> Text.Text
partTwo = Text.pack . show . uncurry solveMath . cephaloParse

solveMath :: [[Int64]] -> [Op] -> Int64
solveMath numbers ops = sum . zipWith calc ops $ numbers

calc :: Op -> [Int64] -> Int64
calc Add = sum
calc Mult = product

parseOps :: Text.Text -> [Op]
parseOps = map (op . Text.head) . filter (not . Text.null) . Text.split (== ' ')
  where
    op '+' = Add
    op '*' = Mult
    op _ = undefined

-- part one

humanParse :: Text.Text -> ([[Int64]], [Op])
humanParse input = (List.transpose . map parseHumanNumbers . init $ rows, parseOps . last $ rows)
  where
    rows = Text.lines input

parseHumanNumbers :: Text.Text -> [Int64]
parseHumanNumbers = map (read . Text.unpack) . filter (not . Text.null) . Text.split (== ' ')

cephaloParse :: Text.Text -> ([[Int64]], [Op])
cephaloParse input = (parseCephaloNumbers . Text.transpose . init $ rows, parseOps . last $ rows)
  where
    rows = Text.lines input

-- part two

parseCephaloNumbers :: [Text.Text] -> [[Int64]]
parseCephaloNumbers = map (map (read . Text.unpack)) . Extra.split Text.null . map Text.strip
