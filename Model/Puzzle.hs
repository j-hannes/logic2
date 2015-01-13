module Model.Puzzle (
  Puzzle(..)
) where

import Model.BlockLenghts
import Model.Cell

data Puzzle = Puzzle {
    horizontalBlocks :: [BlockLenghts]
  , verticalBlocks :: [BlockLenghts]
  , cellMatrix :: [[Cell]]
} deriving Show


