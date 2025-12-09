module Main where

import qualified AoC
import qualified Day09

puzzle :: AoC.Puzzle
puzzle = AoC.Puzzle Day09.solve

main :: IO ()
main = AoC.run puzzle
