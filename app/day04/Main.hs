module Main where

import qualified AoC
import qualified Day04

puzzle :: AoC.Puzzle
puzzle = AoC.Puzzle Day04.solve

main :: IO ()
main = AoC.run puzzle
