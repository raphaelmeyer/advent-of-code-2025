{-# LANGUAGE OverloadedStrings #-}

module Day07 where

import qualified AoC
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text

type Manifold = Set.Set Int

type Beams = Set.Set Int

data Teleporter = Teleporter
  { start :: Int,
    manifolds :: [Manifold]
  }
  deriving (Eq, Show)

solve :: Text.Text -> AoC.Solution
solve input = AoC.Solution (partOne teleporter) "The universe and everything"
  where
    teleporter = parse input

partOne :: Teleporter -> Text.Text
partOne = Text.pack . show . countSplits

-- parse input

parse :: Text.Text -> Teleporter
parse input =
  Teleporter
    { start = findStart (head rows),
      manifolds = map parseManifold (tail rows)
    }
  where
    rows = Text.lines input

findStart :: Text.Text -> Int
findStart input = Maybe.fromMaybe undefined (Text.findIndex (== 'S') input)

parseManifold :: Text.Text -> Manifold
parseManifold = fst . Text.foldl findSplitter (Set.empty, 0)

findSplitter :: (Manifold, Int) -> Char -> (Manifold, Int)
findSplitter (manifold, idx) '^' = (Set.insert idx manifold, idx + 1)
findSplitter (manifold, idx) _ = (manifold, idx + 1)

-- part one

countSplits :: Teleporter -> Int
countSplits teleporter = fst . foldl countSplitsInOneFold (0, Set.singleton $ start teleporter) $ manifolds teleporter

countSplitsInOneFold :: (Int, Beams) -> Manifold -> (Int, Beams)
countSplitsInOneFold (count, beams) manifold = Set.foldl (checkBeam manifold) (count, Set.empty) beams

checkBeam :: Manifold -> (Int, Beams) -> Int -> (Int, Beams)
checkBeam manifold (splits, beams) beam =
  if beam `hitsSplitter` manifold
    then splitBeam (splits, beams) beam
    else passFreely (splits, beams) beam

hitsSplitter :: Int -> Manifold -> Bool
hitsSplitter = Set.member

splitBeam :: (Int, Beams) -> Int -> (Int, Beams)
splitBeam (splits, beams) beam = (splits + 1, Set.insert (beam - 1) . Set.insert (beam + 1) $ beams)

passFreely :: (Int, Beams) -> Int -> (Int, Beams)
passFreely (splits, beams) beam = (splits, Set.insert beam beams)
