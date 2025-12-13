module Main where

import qualified AoC
import qualified Day12

puzzle :: AoC.Puzzle
puzzle = AoC.Puzzle Day12.solve

main :: IO ()
main = AoC.run puzzle
