module Main where

import qualified AoC
import qualified Day07

puzzle :: AoC.Puzzle
puzzle = AoC.Puzzle Day07.solve

main :: IO ()
main = AoC.run puzzle
