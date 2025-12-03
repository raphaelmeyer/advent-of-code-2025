{-# LANGUAGE OverloadedStrings #-}

module Day03 where

import qualified AoC
import qualified Data.Text as Text

solve :: Text.Text -> AoC.Solution
solve input = AoC.Solution (partOne banks) "The universe and everything"
  where
    banks = parseInput input

parseInput :: Text.Text -> [Text.Text]
parseInput = Text.lines

partOne :: [Text.Text] -> Text.Text
partOne input = Text.pack . show . sum $ map ((read :: String -> Int) . maximumPair . Text.unpack) input

maximumPair :: String -> String
maximumPair = unpair . findMaxPair ('0', '0')
  where
    unpair (a, b) = [a, b]

findMaxPair :: (Char, Char) -> String -> (Char, Char)
findMaxPair pair rest = case rest of
  [] -> undefined
  [_] -> pair
  (joltage : rest') -> findMaxPair pair' rest'
    where
      pair' = if joltage > fst pair then (joltage, maximum rest') else pair
