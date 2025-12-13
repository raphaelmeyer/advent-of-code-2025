{-# LANGUAGE OverloadedStrings #-}

module Day12 where

import qualified AoC
import qualified Data.List.Extra as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Vector as Vector

type Shape = Vector.Vector (Vector.Vector Bool)

type Shapes = Map.Map Int Shape

type Presents = Map.Map Int Int

data Tree = Tree {treeArea :: (Int, Int), treePresents :: Presents}
  deriving (Eq, Show)

solve :: Text.Text -> AoC.Solution
solve input = AoC.Solution (partOne shapes trees) "... 🎅"
  where
    (shapes, trees) = parse input

partOne :: Shapes -> [Tree] -> Text.Text
partOne shapes trees = Text.pack . show $ (willFitAnyWay + (length . checkIfFitByArranging . removeThoseThatWontFitWhatever $ needMoreInvestigation))
  where
    checkIfFitByArranging = filter (canArrangePresentsUnderTree shapes)
    removeThoseThatWontFitWhatever = filter (areaCheck shapes)
    needMoreInvestigation = filter (not . fitAnyway) trees
    willFitAnyWay = length trees - length needMoreInvestigation

-- part one

areaCheck :: Shapes -> Tree -> Bool
areaCheck shapes tree = area >= minRequiredArea shapes (treePresents tree)
  where
    area = uncurry (*) . treeArea $ tree

minRequiredArea :: Shapes -> Presents -> Int
minRequiredArea shapes = Map.foldrWithKey accumulateArea 0
  where
    accumulateArea index quantity area = area + quantity * shapeArea (shapes Map.! index)

shapeArea :: Shape -> Int
shapeArea = Vector.foldr accumulate 0
  where
    accumulate row acc = (+ acc) . Vector.length . Vector.filter id $ row

fitAnyway :: Tree -> Bool
fitAnyway tree = sumPresents tree <= count3x3Squares tree

sumPresents :: Tree -> Int
sumPresents = Map.foldr (+) 0 . treePresents

count3x3Squares :: Tree -> Int
count3x3Squares Tree {treeArea = (w, h)} = div w 3 * div h 3

-- It is left as an exercise to the reader to implement a simple optimization
-- algorithm to solve the packing problem with the given 2D shapes.
canArrangePresentsUnderTree :: Shapes -> Tree -> Bool
canArrangePresentsUnderTree _ _ = undefined

-- parse input

parse :: Text.Text -> (Shapes, [Tree])
parse input = (Map.fromList . map parseShape $ init blocks, map parseTree $ last blocks)
  where
    blocks = List.split Text.null . Text.lines $ input

parseShape :: [Text.Text] -> (Int, Shape)
parseShape input = (index, shape)
  where
    index = read . Text.unpack . Text.filter (/= ':') . head $ input
    shape = foldr (Vector.cons . row) Vector.empty . tail $ input
    row = Text.foldr (Vector.cons . present) Vector.empty
    present '#' = True
    present _ = False

parseTree :: Text.Text -> Tree
parseTree input = Tree (parseArea . head $ parts) (Map.fromList $ zip [0 ..] (map (read . Text.unpack) $ tail parts))
  where
    parts = Text.words input
    parseArea = apply (read . Text.unpack . Text.filter (/= 'x')) . Text.break (== 'x') . Text.filter (/= ':')
    apply f (a, b) = (f a, f b)
