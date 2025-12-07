module Main where

import qualified AoC
import qualified Day08

puzzle :: AoC.Puzzle
puzzle = AoC.Puzzle Day08.solve

main :: IO ()
main = AoC.run puzzle
