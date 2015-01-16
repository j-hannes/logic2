module Combinator2 (
    calculateCombinations
) where

import Control.Monad (replicateM)

import Model.Cell
import Model.Puzzle
import Model.Row
import Permutator

type CellArray = [Cell]

allCombinationsMatching :: Row -> CellArray -> [CellArray]
allCombinationsMatching row config = undefined
    --configVariationsMatching 

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
gapVariations space blocks =
    addMinLengths $ filter (\x -> sum x == v) permutations
  where
    v = variability space blocks
    permutations = e2 gapCount v
    gapCount = length blocks + 1
    addMinLengths = map . zipWith (+) $ gapMinLengths blocks

variability :: Int -> [Int] -> Int
variability space blocks = space - sum blocks - length blocks + 1

gapMinLengths :: [Int]-> [Int]
gapMinLengths blocks = 0 : replicate (length blocks - 1) 1 ++ [0]

