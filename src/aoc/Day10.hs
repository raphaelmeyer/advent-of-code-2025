{-# LANGUAGE OverloadedStrings #-}

module Day10 where

import qualified AoC
import Control.Applicative ((<|>))
import qualified Data.Bits as Bits
import qualified Data.Char as Char
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Text.ParserCombinators.ReadP as ReadP

data Machine = Machine
  { mIndicator :: Int,
    mButtons :: [Int]
  }
  deriving (Eq, Show)

solve :: Text.Text -> AoC.Solution
solve input = AoC.Solution (partOne machines) "The universe and everything"
  where
    machines = parse input

partOne :: [Machine] -> Text.Text
partOne = Text.pack . show . sum . map countButtonPress

-- part one

countButtonPress :: Machine -> Int
countButtonPress machine =
  Maybe.fromMaybe undefined $
    pressButtons (mIndicator machine) (mButtons machine) 0

pressButtons :: Int -> [Int] -> Int -> Maybe Int
pressButtons goal [] indicator =
  if goal == indicator then Just 0 else Nothing
pressButtons goal (button : buttons) indicator
  | indicator == goal = Just 0
  | otherwise = case (doPress, doNotPress) of
      (Just yes, Just no) -> Just $ min (yes + 1) no
      (Just yes, _) -> Just (1 + yes)
      (_, Just no) -> Just no
      _ -> Nothing
  where
    doPress = pressButtons goal buttons $ Bits.xor indicator button
    doNotPress = pressButtons goal buttons indicator

-- parse input

parse :: Text.Text -> [Machine]
parse = map parseMachine . Text.lines

parseMachine :: Text.Text -> Machine
parseMachine input = case ReadP.readP_to_S pMachine $ Text.unpack input of
  [(result, _)] -> result
  x -> error . show $ x

pMachine :: ReadP.ReadP Machine
pMachine =
  Machine
    <$> pIndicator
    <*> pButtons
    <* pJoltage
    <* ReadP.skipSpaces
    <* ReadP.eof

pIndicator :: ReadP.ReadP Int
pIndicator = do
  diagram <-
    ReadP.between (pConsume '[') (pConsume ']') $
      ReadP.many1 (ReadP.char '.' <|> ReadP.char '#')
  pure $ decodeDiagram diagram

pButtons :: ReadP.ReadP [Int]
pButtons = ReadP.many1 pButton

pJoltage :: ReadP.ReadP ()
pJoltage = do
  _ <-
    ReadP.between (pConsume '{') (pConsume '}') $
      ReadP.sepBy pNumber pComma
  pure ()

pButton :: ReadP.ReadP Int
pButton = do
  bs <-
    ReadP.between (pConsume '(') (pConsume ')') $
      ReadP.sepBy pNumber pComma
  pure $ decodeButton bs

pNumber :: ReadP.ReadP Int
pNumber = do
  digits <- ReadP.munch1 Char.isDigit
  pure $ read digits

pComma :: ReadP.ReadP ()
pComma = ReadP.skipSpaces <* ReadP.char ','

pConsume :: Char -> ReadP.ReadP ()
pConsume c = ReadP.skipSpaces <* ReadP.char c

decodeDiagram :: String -> Int
decodeDiagram = foldr setBit 0
  where
    setBit c n = case c of
      '#' -> (`Bits.setBit` 0) . (`Bits.shiftL` 1) $ n
      _ -> Bits.shiftL n 1

decodeButton :: [Int] -> Int
decodeButton = foldr (flip Bits.setBit) 0
