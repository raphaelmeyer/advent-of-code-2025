{-# LANGUAGE OverloadedStrings #-}

module Day09 where

import qualified AoC
import qualified Data.List as List
import qualified Data.Text as Text

data Corner = Corner Int Int
  deriving (Eq, Show)

data Rectangle = Rectangle Corner Corner
  deriving (Eq, Show)

data Vertical = Vertical {verticalX :: Int, verticalTop :: Int, verticalBottom :: Int}
  deriving (Eq, Show)

data Horizontal = Horizontal {horizontalY :: Int, horizontalLeft :: Int, horizontalRight :: Int}
  deriving (Eq, Show)

solve :: Text.Text -> AoC.Solution
solve input = AoC.Solution (partOne rectangles) (partTwo corners)
  where
    corners = parse input
    rectangles = allRectangles corners

partOne :: [Rectangle] -> Text.Text
partOne = Text.pack . show . largestArea

partTwo :: [Corner] -> Text.Text
partTwo = Text.pack . show . largestAreaInside

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

-- part two

largestAreaInside :: [Corner] -> Int
largestAreaInside corners = case List.find (isInside verticals horizontals . snd) sorted of
  Just r -> fst r
  _ -> undefined
  where
    sorted = sortedByArea . normalizeRectangles $ rectangles
    rectangles = allRectangles corners
    verticals = allVerticals corners
    horizontals = allHorizontals corners

isInside :: [Vertical] -> [Horizontal] -> Rectangle -> Bool
isInside verticals horizontals rectangle =
  rayCastOnLeft verticals rectangle
    && noCrossings rectangle verticals horizontals

rayCastOnLeft :: [Vertical] -> Rectangle -> Bool
rayCastOnLeft verticals rectangle = all (odd . countVerticalsOnPoint onLeft) points
  where
    points = verticalPointsToCheck rectangle onLeft
    onLeft = verticalsOnLeft rectangle . verticalsWithoutBottom $ verticals

noCrossings :: Rectangle -> [Vertical] -> [Horizontal] -> Bool
noCrossings rectangle verticals horizontals =
  and
    [ not . any (crossingVertical rectangle) $ verticals,
      not . any (crossingVertical rectangle) $ verticals,
      not . any (crossingHorizontal rectangle) $ horizontals,
      not . any (crossingHorizontal rectangle) $ horizontals
    ]

verticalsWithoutBottom :: [Vertical] -> [Vertical]
verticalsWithoutBottom = map cutBottom
  where
    cutBottom (Vertical x top bottom) =
      if top < bottom
        then Vertical x top (bottom - 1)
        else undefined

verticalsOnLeft :: Rectangle -> [Vertical] -> [Vertical]
verticalsOnLeft (Rectangle (Corner ax _) (Corner _ _)) = filter ((<= ax) . verticalX)

verticalPointsToCheck :: Rectangle -> [Vertical] -> [Int]
verticalPointsToCheck (Rectangle (Corner _ ay) (Corner _ by)) = foldl isPointToCheck rectangleTopBottom
  where
    rectangleTopBottom = if ay < by then [ay, by - 1] else [ay]
    isPointToCheck points (Vertical _ top bottom) =
      case (overlap top, overlap bottom) of
        (True, True) -> top : bottom : points
        (True, False) -> top : points
        (False, True) -> bottom : points
        _ -> points
    overlap y = ay <= y && y <= by

countVerticalsOnPoint :: [Vertical] -> Int -> Int
countVerticalsOnPoint verticals point = foldl overlaps 0 verticals
  where
    overlaps count (Vertical _ top bottom) =
      if top <= point && point <= bottom
        then count + 1
        else count

crossingVertical :: Rectangle -> Vertical -> Bool
crossingVertical (Rectangle (Corner ax ay) (Corner bx by)) (Vertical x top bottom) =
  (ax < x && x < bx)
    && ((top <= ay && ay < bottom) || (top < by && by <= bottom))

crossingHorizontal :: Rectangle -> Horizontal -> Bool
crossingHorizontal (Rectangle (Corner ax ay) (Corner bx by)) (Horizontal y left right) =
  (ay < y && y < by)
    && ((left <= ax && ax < right) || (left < bx && bx <= right))

allVerticals :: [Corner] -> [Vertical]
allVerticals corners = foldl mkVertical [] $ zip corners (last corners : init corners)
  where
    mkVertical verticals (Corner ax ay, Corner bx by)
      | ax == bx && ay < by = Vertical ax ay by : verticals
      | ax == bx = Vertical ax by ay : verticals
      | otherwise = verticals

allHorizontals :: [Corner] -> [Horizontal]
allHorizontals corners = foldl mkHorizontal [] $ zip corners (last corners : init corners)
  where
    mkHorizontal horizontals (Corner ax ay, Corner bx by)
      | ay == by && ax < bx = Horizontal ay ax bx : horizontals
      | ay == by = Horizontal ay bx ax : horizontals
      | otherwise = horizontals

sortedByArea :: [Rectangle] -> [(Int, Rectangle)]
sortedByArea = List.sortBy (\a b -> compare (fst b) (fst a)) . map (\r -> (rectangleArea r, r))

normalizeRectangles :: [Rectangle] -> [Rectangle]
normalizeRectangles = map normalize
  where
    normalize (Rectangle (Corner ax ay) (Corner bx by)) = case (ax < bx, ay < by) of
      (True, True) -> Rectangle (Corner ax ay) (Corner bx by)
      (True, False) -> Rectangle (Corner ax by) (Corner bx ay)
      (False, True) -> Rectangle (Corner bx ay) (Corner ax by)
      (False, False) -> Rectangle (Corner bx by) (Corner ax ay)
