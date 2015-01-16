module Model.Puzzle (
  Puzzle(..)
) where

import Model.Cell (Cell)

data Puzzle = Puzzle {
    horizontalBlocks :: [[Int]]
  , verticalBlocks :: [[Int]]
  , cellMatrix :: [[Cell]]
} deriving Show


