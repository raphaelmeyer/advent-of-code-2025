{-# LANGUAGE OverloadedStrings #-}

module Day11 where

import qualified AoC
import qualified Control.Monad.State as State
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

type Outputs = Map.Map Text.Text (Set.Set Text.Text)

solve :: Text.Text -> AoC.Solution
solve input = AoC.Solution (partOne outputs) (partTwo outputs)
  where
    outputs = parse input

partOne :: Outputs -> Text.Text
partOne = Text.pack . show . countPaths

partTwo :: Outputs -> Text.Text
partTwo = Text.pack . show . countDacAndFftPath

-- part one

countPaths :: Outputs -> Int
countPaths outputs = searchPath outputs [] $ "you"

searchPath :: Outputs -> [Text.Text] -> Text.Text -> Int
searchPath _ _ "out" = 1
searchPath outputs path current = maybe 0 followPaths (Map.lookup current outputs)
  where
    followPaths = sum . map (searchPath outputs (current : path)) . filter (`notElem` path) . Set.toList

-- part two

type Memo = Map.Map (Text.Text, Bool, Bool) Int

countDacAndFftPath :: Outputs -> Int
countDacAndFftPath outputs = State.evalState search Map.empty
  where
    search = searchDacAndFftPath outputs [] "svr"

searchDacAndFftPath :: Outputs -> [Text.Text] -> Text.Text -> State.State Memo Int
searchDacAndFftPath _ path "out" = pure $ if visitsDacAndFft path then 1 else 0
searchDacAndFftPath outputs path current = do
  memo <- State.get
  let key = (current, elem "dac" path, elem "fft" path)
  case Map.lookup key memo of
    Just numPaths -> pure numPaths
    Nothing -> do
      case Map.lookup current outputs of
        Just connections -> do
          let candidates = filter (`notElem` path) . Set.toList $ connections
          count <- sum <$> mapM (searchDacAndFftPath outputs (current : path)) candidates
          State.modify (Map.insert key count)
          pure count
        Nothing -> pure 0

visitsDacAndFft :: [Text.Text] -> Bool
visitsDacAndFft path = elem "dac" path && elem "fft" path

-- parse input

parse :: Text.Text -> Outputs
parse = Map.fromList . map parseDevice . Text.lines

parseDevice :: Text.Text -> (Text.Text, Set.Set Text.Text)
parseDevice input = (device, Set.fromList . Text.words $ outputs)
  where
    (device, outputs) = Text.break (== ':') input
