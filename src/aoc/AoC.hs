{-# LANGUAGE OverloadedStrings #-}

module AoC (Puzzle (..), Solution (..), run) where

import qualified Data.Text as Text
import qualified System.Environment as SysEnv

data Solution = Solution {one :: Text.Text, two :: Text.Text}

data Puzzle = Puzzle
  { solve :: Text.Text -> Solution
  }

run :: Puzzle -> IO ()
run puzzle = do
  args <- SysEnv.getArgs
  case args of
    [file] -> do
      input <- Text.pack <$> readFile file
      let solution = solve puzzle input
      printSolution solution
    _ -> putStrLn "Input file missing"

printSolution :: Solution -> IO ()
printSolution solution = do
  putStrLn $ Text.unpack . Text.concat $ ["⭐ Part One : ", one solution]
  putStrLn $ Text.unpack . Text.concat $ ["⭐ Part Two : ", two solution]
