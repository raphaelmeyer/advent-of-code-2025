{-# LANGUAGE OverloadedStrings #-}

module Day10 where

import qualified AoC
import Control.Applicative ((<|>))
import qualified Control.Monad.State as State
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Text.ParserCombinators.ReadP as ReadP

type Indicator = [Bool]

type Button = [Bool]

type Joltage = [Int]

data Machine = Machine
  { mIndicator :: Indicator,
    mButtons :: [Button],
    mJoltage :: Joltage
  }
  deriving (Eq, Show)

solve :: Text.Text -> AoC.Solution
solve input = AoC.Solution (partOne machines) (partTwo machines)
  where
    machines = parse input

partOne :: [Machine] -> Text.Text
partOne = Text.pack . show . sum . map countButtonPress

partTwo :: [Machine] -> Text.Text
partTwo = Text.pack . show . sum . map countJoltageButtonPresses

-- part one

countButtonPress :: Machine -> Int
countButtonPress machine =
  minimum . map length $
    findButtonsForIndicator (mIndicator machine) (mButtons machine) initial
  where
    initial = replicate (length . mIndicator $ machine) False

findButtonsForIndicator :: Indicator -> [Button] -> Indicator -> [[Button]]
findButtonsForIndicator expected [] current = [[] | expected == current]
findButtonsForIndicator expected (button : buttons) current =
  map (button :) doPress ++ doNotPress
  where
    doPress = findButtonsForIndicator expected buttons $ pressButton button current
    doNotPress = findButtonsForIndicator expected buttons current

pressButton :: Button -> Indicator -> Indicator
pressButton = zipWith (\toggle light -> if toggle then not light else light)

-- part two

data Memo = Memo
  { memoCounts :: Map.Map Joltage (Maybe Int),
    memoButtons :: Map.Map Indicator [[Button]]
  }

countJoltageButtonPresses :: Machine -> Int
countJoltageButtonPresses machine =
  Maybe.fromJust $
    State.evalState startSearch (Memo Map.empty Map.empty)
  where
    startSearch = search (mButtons machine) (mJoltage machine)

search :: [Button] -> Joltage -> State.State Memo (Maybe Int)
search buttons joltage
  | all (== 0) joltage = pure $ Just 0
  | any (< 0) joltage = undefined -- validated after un-applying buttons
  | otherwise = do
      memo <- State.gets memoCounts
      case Map.lookup joltage memo of
        Just count -> pure count
        Nothing -> do
          candidates <- findCandidates buttons joltage
          searchCandidates <- mapM (search buttons . snd) candidates
          let maybeCounts = zipWith sumPresses (map fst candidates) searchCandidates
          case Maybe.catMaybes maybeCounts of
            [] -> pure Nothing
            counts -> do
              let minCount = Just $ minimum counts
              State.modify (\s -> s {memoCounts = Map.insert joltage minCount $ memoCounts s})
              pure minCount
  where
    sumPresses count = fmap ((+ count) . (* 2))

findCandidates :: [Button] -> Joltage -> State.State Memo [(Int, Joltage)]
findCandidates buttons joltage = do
  let oddJoltage = map odd joltage
  memo <- State.gets memoButtons
  presses <- case Map.lookup oddJoltage memo of
    Just combinations -> pure combinations
    Nothing -> do
      let combinations = findButtonsForIndicator (map odd joltage) buttons lookForAllEven
      State.modify (\s -> s {memoButtons = Map.insert oddJoltage combinations $ memoButtons s})
      pure combinations
  pure
    . halveJoltages
    . removeInvalid
    . map (unapplyButtons . addNumPress)
    $ presses
  where
    lookForAllEven = replicate (length joltage) False
    addNumPress candidate = (length candidate, candidate)
    unapplyButtons = withSnd (foldr unpressJoltageButton joltage)
    removeInvalid = filter (all (>= 0) . snd)
    halveJoltages = map (withSnd halve)
    halve = map (`div` 2)

unpressJoltageButton :: Button -> Joltage -> Joltage
unpressJoltageButton = zipWith (\toggle joltage -> if toggle then joltage - 1 else joltage)

withSnd :: (b -> c) -> (a, b) -> (a, c)
withSnd f (a, b) = (a, f b)

-- parse input

parse :: Text.Text -> [Machine]
parse = map parseMachine . Text.lines

parseMachine :: Text.Text -> Machine
parseMachine input = case ReadP.readP_to_S pMachine $ Text.unpack input of
  [(result, _)] -> result
  x -> error . show $ x

pMachine :: ReadP.ReadP Machine
pMachine = do
  indicator <- pIndicator
  buttons <- pButtons (length indicator)
  joltage <- pJoltage
  _ <- ReadP.skipSpaces <* ReadP.eof
  pure $ Machine indicator buttons joltage

pIndicator :: ReadP.ReadP Indicator
pIndicator = do
  diagram <-
    ReadP.between (pConsume '[') (pConsume ']') $
      ReadP.many1 (ReadP.char '.' <|> ReadP.char '#')
  pure $ decodeDiagram diagram

pButtons :: Int -> ReadP.ReadP [Button]
pButtons n = ReadP.many1 (pButton n)

pJoltage :: ReadP.ReadP Joltage
pJoltage =
  ReadP.between (pConsume '{') (pConsume '}') $
    ReadP.sepBy pNumber pComma

pButton :: Int -> ReadP.ReadP Button
pButton n = do
  bs <-
    ReadP.between (pConsume '(') (pConsume ')') $
      ReadP.sepBy pNumber pComma
  pure $ decodeButton n bs

pNumber :: ReadP.ReadP Int
pNumber = do
  digits <- ReadP.munch1 Char.isDigit
  pure $ read digits

pComma :: ReadP.ReadP ()
pComma = ReadP.skipSpaces <* ReadP.char ','

pConsume :: Char -> ReadP.ReadP ()
pConsume c = ReadP.skipSpaces <* ReadP.char c

decodeDiagram :: String -> Indicator
decodeDiagram = map (== '#')

decodeButton :: Int -> [Int] -> Button
decodeButton n toggles = map (`elem` toggles) [0 .. n - 1]
