module BoardGenerator (makeBoard) where

import Types.Board (Board, Grid(Grid))
import qualified Types.Board as Board

import Types.Row (Row(Row))
import qualified Types.Row as Row

import Types.Block (Block(Block))

import Types.Cell (Cell(..))

makeBoard :: Board.Spec -> Board
makeBoard (rows, columns) =
    Grid {
        Board.rows = map (makeRow $ length columns) rows
      , Board.columns = map (makeRow $ length rows) columns
    }
  
makeRow :: Int -> [Int] -> Row
makeRow rowLength blocks = Row {
        Row.blocks = map (makeBlock margin) blocks
      , Row.cells = replicate rowLength Unknown
      , Row.margin = margin
      , Row.hasOverlap = margin < maximum blocks
    }
  where
    margin = calculateMargin rowLength blocks
    
calculateMargin :: Int -> [Int] -> Int
calculateMargin rowLength blockLengths =
    rowLength - (sum blockLengths + numberOfBlocks - 1)
  where
    numberOfBlocks = length blockLengths 

makeBlock :: Int -> Int -> Block
makeBlock margin size = Block size (size > margin)

