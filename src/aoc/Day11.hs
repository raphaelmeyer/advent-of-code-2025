{-# LANGUAGE OverloadedStrings #-}

module Day11 where

import qualified AoC
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

type Outputs = Map.Map Text.Text (Set.Set Text.Text)

solve :: Text.Text -> AoC.Solution
solve input = AoC.Solution (partOne outputs) "The universe and everything"
  where
    outputs = parse input

partOne :: Outputs -> Text.Text
partOne = Text.pack . show . countPaths

-- part one

countPaths :: Outputs -> Int
countPaths outputs = length . searchPath outputs [] $ "you"

searchPath :: Outputs -> [Text.Text] -> Text.Text -> [[Text.Text]]
searchPath outputs path current =
  if current == "out"
    then [path]
    else case Map.lookup current outputs of
      Just connections -> (concatMap (searchPath outputs (current : path)) . filter (`notElem` path) . Set.toList) $ connections
      Nothing -> []

-- parse input

parse :: Text.Text -> Outputs
parse = Map.fromList . map parseDevice . Text.lines

parseDevice :: Text.Text -> (Text.Text, Set.Set Text.Text)
parseDevice input = (device, Set.fromList . Text.words $ outputs)
  where
    (device, outputs) = Text.break (== ':') input
