module Main where

import qualified AoC
import qualified Day01

puzzle :: AoC.Puzzle
puzzle = AoC.Puzzle Day01.solve

main :: IO ()
main = AoC.run puzzle
