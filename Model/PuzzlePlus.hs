module Model.PuzzlePlus (
    PuzzlePlus(..)
) where

import Model.Cell
import Model.Row
-- import Controller.Puzzle(parseConfig)

data PuzzlePlus = PuzzlePlus {
    horizontalRows :: [Row]
  , verticalRows :: [Row]
  , cellMatrix :: [[Cell]]
} deriving Show

