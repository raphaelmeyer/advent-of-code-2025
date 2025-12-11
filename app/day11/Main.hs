module Main where

import qualified AoC
import qualified Day11

puzzle :: AoC.Puzzle
puzzle = AoC.Puzzle Day11.solve

main :: IO ()
main = AoC.run puzzle
