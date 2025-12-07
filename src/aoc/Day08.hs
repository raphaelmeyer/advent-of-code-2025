{-# LANGUAGE OverloadedStrings #-}

module Day08 where

import qualified AoC
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

data Position = Position Int Int Int
  deriving (Eq, Ord, Show)

data Pair = Pair Position Position
  deriving (Eq, Ord, Show)

type Boxes = Set.Set Position

type Distances = Map.Map Int Pair

type Circuit = Set.Set Position

type Circuits = Map.Map Int Circuit

solve :: Text.Text -> AoC.Solution
solve input = AoC.Solution (partOne boxes) "The universe and everything"
  where
    boxes = parse input

partOne :: Boxes -> Text.Text
partOne = Text.pack . show . largestCircuits 1000

-- parse input

parse :: Text.Text -> Boxes
parse = Set.fromList . map parsePosition . Text.lines

parsePosition :: Text.Text -> Position
parsePosition input = case coordAsList of
  [x, y, z] -> Position x y z
  _ -> undefined
  where
    coordAsList = map (read . Text.unpack) . Text.splitOn "," $ input

-- part one

largestCircuits :: Int -> Boxes -> Int
largestCircuits n boxes = product . take 3 . reverse . List.sort . map (fromIntegral . Set.size) . Map.elems $ connected
  where
    connected = connectTimes n circuits ds
    ds = distances . uniquePairs $ boxes
    circuits = unconnectedCircuits boxes

connectTimes :: Int -> Circuits -> Distances -> Circuits
connectTimes n circuits ds = foldTimes n connect circuits $ pairsSortedByDistance
  where
    pairsSortedByDistance = map (ds Map.!) $ sortedDistances ds

sortedDistances :: Distances -> [Int]
sortedDistances = List.sort . Map.keys

connect :: Circuits -> Pair -> Circuits
connect circuits (Pair a b) =
  if aId == bId
    then circuits
    else merge circuits
  where
    (aId, aCircuit) = findCircuit a circuits
    (bId, bCircuit) = findCircuit b circuits
    merge = Map.delete bId . Map.insert aId (Set.union aCircuit bCircuit)

findCircuit :: Position -> Circuits -> (Int, Circuit)
findCircuit box = head . Map.toList . Map.filter (Set.member box)

uniquePairs :: Boxes -> Set.Set Pair
uniquePairs boxes = Set.fromList $ [Pair a b | (a : js) <- List.tails sorted, b <- js]
  where
    sorted = Set.toAscList boxes

unconnectedCircuits :: Boxes -> Circuits
unconnectedCircuits boxes = Map.fromList $ zip [1 ..] (map Set.singleton . Set.toList $ boxes)

distances :: Set.Set Pair -> Distances
distances = foldl addDistance Map.empty
  where
    addDistance ds pair = Map.insert (distance pair) pair ds

distance :: Pair -> Int
distance (Pair (Position ax ay az) (Position bx by bz)) =
  dx * dx + dy * dy + dz * dz
  where
    dx = ax - bx
    dy = ay - by
    dz = az - bz

foldTimes :: Int -> (a -> b -> a) -> a -> [b] -> a
foldTimes _ _ acc [] = acc
foldTimes 0 _ acc _ = acc
foldTimes n f acc (x : xs) = foldTimes (n - 1) f (f acc x) xs
