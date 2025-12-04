module Main where

import qualified AoC
import qualified Day05

puzzle :: AoC.Puzzle
puzzle = AoC.Puzzle Day05.solve

main :: IO ()
main = AoC.run puzzle
