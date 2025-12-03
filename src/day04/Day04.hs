{-# LANGUAGE OverloadedStrings #-}

module Day04 where

import qualified AoC
import qualified Data.Set as Set
import qualified Data.Text as Text

data Position = Position Int Int
  deriving (Eq, Ord, Show)

type Grid = Set.Set Position

solve :: Text.Text -> AoC.Solution
solve input = AoC.Solution (partOne grid) "The universe and everything"
  where
    grid = parse input

partOne :: Grid -> Text.Text
partOne = Text.pack . show . countAccessible

parse :: Text.Text -> Grid
parse = fst . foldl parseLine (Set.empty, 0) . Text.lines

parseLine :: (Grid, Int) -> Text.Text -> (Grid, Int)
parseLine (grid, y) line = (grid', y + 1)
  where
    (grid', _, _) = foldl parsePosition (grid, 0, y) . Text.unpack $ line

parsePosition :: (Grid, Int, Int) -> Char -> (Grid, Int, Int)
parsePosition (grid, x, y) '@' = (Set.insert (Position x y) grid, x + 1, y)
parsePosition (grid, x, y) _ = (grid, x + 1, y)

countAccessible :: Grid -> Int
countAccessible grid = Set.size $ Set.filter ((< 4) . countNeighbors grid) $ grid

countNeighbors :: Grid -> Position -> Int
countNeighbors grid position = length . filter (`Set.member` grid) $ possibleNeighbors position

possibleNeighbors :: Position -> [Position]
possibleNeighbors (Position x y) =
  [ Position (x - 1) (y - 1),
    Position (x) (y - 1),
    Position (x + 1) (y - 1),
    Position (x - 1) (y),
    Position (x + 1) (y),
    Position (x - 1) (y + 1),
    Position (x) (y + 1),
    Position (x + 1) (y + 1)
  ]
