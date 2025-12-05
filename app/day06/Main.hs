module Main where

import qualified AoC
import qualified Day06

puzzle :: AoC.Puzzle
puzzle = AoC.Puzzle Day06.solve

main :: IO ()
main = AoC.run puzzle
