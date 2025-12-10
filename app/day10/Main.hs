module Main where

import qualified AoC
import qualified Day10

puzzle :: AoC.Puzzle
puzzle = AoC.Puzzle Day10.solve

main :: IO ()
main = AoC.run puzzle
