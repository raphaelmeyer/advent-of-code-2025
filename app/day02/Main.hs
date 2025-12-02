module Main where

import qualified AoC
import qualified Day02

puzzle :: AoC.Puzzle
puzzle = AoC.Puzzle Day02.solve

main :: IO ()
main = AoC.run puzzle
