module Main where

import qualified AoC
import qualified Day00

puzzle :: AoC.Puzzle
puzzle = AoC.Puzzle Day00.solve

main :: IO ()
main = AoC.run puzzle
