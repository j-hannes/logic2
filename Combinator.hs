module Combinator (
    calculateCombinations
) where

import Model.Cell (Cell(..))
import Model.Puzzle (Puzzle(..))
import qualified Permutator as P

type CellArray = [Cell]

calculateCombinations :: Puzzle -> ([[CellArray]], [[CellArray]])
calculateCombinations puzzle =
    (combine width horizontal, combine height vertical) 
  where
    horizontal = horizontalBlocks puzzle
    vertical = verticalBlocks puzzle
    height = length horizontal
    width = length vertical
    combine l = map $ map interpretConfig . configVariations l

interpretConfig :: [Int] -> CellArray
interpretConfig [x] = replicate x Blank
interpretConfig (x:y:xs) =
    replicate x Blank ++ replicate y Filled ++ interpretConfig xs

configVariations :: Int -> [Int] -> [[Int]]
configVariations space blocks =
    map (mergeBlocks blocks) $ gapVariations space blocks 

mergeBlocks :: [Int] -> [Int] -> [Int]
mergeBlocks [] [gap] = [gap]
mergeBlocks (block:blocks) (gap:gaps) =
    gap : block : mergeBlocks blocks gaps

gapVariations :: Int -> [Int] -> [[Int]]
gapVariations space blocks = addMinLengths permutations
  where
    v = variability space blocks
    permutations = P.e4 v gapCount
    gapCount = length blocks + 1
    addMinLengths = map . zipWith (+) $ gapMinLengths blocks

variability :: Int -> [Int] -> Int
variability space blocks = space - sum blocks - length blocks + 1

gapMinLengths :: [Int]-> [Int]
gapMinLengths blocks = 0 : replicate (length blocks - 1) 1 ++ [0]

