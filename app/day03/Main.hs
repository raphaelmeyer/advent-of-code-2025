module Main where

import qualified AoC
import qualified Day03

puzzle :: AoC.Puzzle
puzzle = AoC.Puzzle Day03.solve

main :: IO ()
main = AoC.run puzzle
