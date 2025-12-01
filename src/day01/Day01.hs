{-# LANGUAGE OverloadedStrings #-}

module Day01 where

import qualified AoC
import qualified Data.Text as Text

data Rotation = RotLeft Int | RotRight Int
  deriving (Eq, Show)

type Position = Int

type Count = Int

solve :: Text.Text -> AoC.Solution
solve input = AoC.Solution one "The universe and everything"
  where
    rotations = parse input
    one = Text.pack . show . countZero $ rotations

parse :: Text.Text -> [Rotation]
parse =
  map parseRotation
    . filter (not . Text.null)
    . Text.split (== '\n')

parseRotation :: Text.Text -> Rotation
parseRotation input = case Text.uncons input of
  Just ('L', distance) -> RotLeft . read . Text.unpack $ distance
  Just ('R', distance) -> RotRight . read . Text.unpack $ distance
  _ -> undefined

countZero :: [Rotation] -> Int
countZero rotations = snd $ foldl rotCountZeroEnd (50, 0) rotations

rotCountZeroEnd :: (Position, Count) -> Rotation -> (Position, Count)
rotCountZeroEnd (position, count) rotation = case rot position rotation of
  0 -> (0, count + 1)
  p -> (p, count)

rot :: Position -> Rotation -> Position
rot p (RotLeft d) = mod (p - d) 100
rot p (RotRight d) = mod (p + d) 100
