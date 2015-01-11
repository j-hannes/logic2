module BoardGenerator (makeBoard) where

import Types.Board (Board(Board))
import qualified Types.Board as Board

import Types.Row (Row(Row))
import qualified Types.Row as Row

import Types.Block (Block(Block))

import Types.Cell (Cell(..))

makeBoard :: Board.Spec -> Board
makeBoard (rows, columns) =
    Board {
        Board.rows = map (makeRow $ length columns) rows
      , Board.columns = map (makeRow $ length rows) columns
    }
  
makeRow :: Int -> [Int] -> Row
makeRow size blocks = Row {
        Row.blocks = map (makeBlock margin) blocks
      , Row.cells = replicate size Unknown
      , Row.margin = margin
      , Row.hasOverlap = margin < maximum blocks
    }
  where
    margin = calculateMargin size blocks
    
calculateMargin :: Int -> [Int] -> Int
calculateMargin size blocks = size - sum blocks - length blocks + 1

makeBlock :: Int -> Int -> Block
makeBlock margin size = Block size (size > margin)

