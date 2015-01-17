module Model.PuzzlePlus (
    PuzzlePlus(..)
) where

import Model.Cell
import Model.Row
-- import Controller.Puzzle(parseConfig)

data PuzzlePlus = PuzzlePlus {
    cbs :: ([[[Cell]]],[[[Cell]]])
  , horizontalRows :: [Row]
  , verticalRows :: [Row]
  , cellMatrix :: [[Cell]]
}

instance Show PuzzlePlus where
  show (PuzzlePlus _ _ _ cm) =
    drawLine (replicate (length (head cm)) Blank) ++
    concatMap drawLine cm ++
    drawLine (replicate (length (head cm)) Blank) ++ "\n"

drawLine :: [Cell] -> String
drawLine cl =
  coloredFields Blank ++
  concatMap coloredFields cl ++
  coloredFields Blank ++ "\n"

coloredFields :: Cell -> String
coloredFields Blank = show White
coloredFields Filled = show Black
coloredFields Unknown = show Green

data ColorBlock =
    Black | White | Cyan | Purple | Blue | Yellow | Green | Red
  deriving (Enum, Eq)

instance Show ColorBlock where
    show Black = "  "
    show White = "\ESC[0;47m  \ESC[m"
    show Cyan = "\ESC[0;46m  \ESC[m"
    show Purple = "\ESC[0;45m  \ESC[m"
    show Blue = "\ESC[0;44m  \ESC[m"
    show Yellow = "\ESC[0;43m  \ESC[m"
    show Green = "\ESC[0;42m  \ESC[m"
    show Red = "\ESC[0;41m  \ESC[m"

