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
solve input = AoC.Solution (partOne pairs circuits) (partTwo pairs circuits)
  where
    (pairs, circuits) = prepare . parse $ input

partOne :: [Pair] -> Circuits -> Text.Text
partOne pairs = Text.pack . show . largestCircuits 1000 pairs

partTwo :: [Pair] -> Circuits -> Text.Text
partTwo pairs = Text.pack . show . lastConnection pairs

-- parse input

parse :: Text.Text -> Boxes
parse = Set.fromList . map parsePosition . Text.lines

parsePosition :: Text.Text -> Position
parsePosition input = case coordAsList of
  [x, y, z] -> Position x y z
  _ -> undefined
  where
    coordAsList = map (read . Text.unpack) . Text.splitOn "," $ input

prepare :: Boxes -> ([Pair], Circuits)
prepare boxes = (pairsSortedByDistance, circuits)
  where
    circuits = unconnectedCircuits boxes
    pairsSortedByDistance = map (ds Map.!) $ sortedDistances ds
    ds = distances . uniquePairs $ boxes

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

-- part one

largestCircuits :: Int -> [Pair] -> Circuits -> Int
largestCircuits n pairs circuits = product . largest $ connected
  where
    largest = take 3 . reverse . List.sort . map (fromIntegral . Set.size) . Map.elems
    connected = connectTimes n pairs circuits

connectTimes :: Int -> [Pair] -> Circuits -> Circuits
connectTimes n pairs circuits = foldTimes n connect circuits pairs

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

foldTimes :: Int -> (a -> b -> a) -> a -> [b] -> a
foldTimes _ _ acc [] = acc
foldTimes 0 _ acc _ = acc
foldTimes n f acc (x : xs) = foldTimes (n - 1) f (f acc x) xs

-- part two

lastConnection :: [Pair] -> Circuits -> Int
lastConnection pairs circuits = a * b
  where
    (Pair (Position a _ _) (Position b _ _)) = connectAll circuits pairs

connectAll :: Circuits -> [Pair] -> Pair
connectAll _ [] = undefined
connectAll circuits (Pair a b : pairs)
  | aId == bId = connectAll circuits pairs
  | Map.size merged == 1 = Pair a b
  | otherwise = connectAll merged pairs
  where
    (aId, aCircuit) = findCircuit a circuits
    (bId, bCircuit) = findCircuit b circuits
    merged =
      Map.delete bId . Map.insert aId (Set.union aCircuit bCircuit) $ circuits
