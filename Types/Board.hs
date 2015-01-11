module Types.Board (
    Board(..)
  , Spec
  ) where

import Types.Row

data Board = Board {
    rows :: [Row]
  , columns :: [Row]
}

instance Show Board where
  show (Board _rows _cols) = showTitle ++ showRows ++ showColumns
    where
      showTitle = "BOARD (" ++ show width ++ "x" ++ show height ++ ")\n"
      showRows = "rows:\n" ++ concatMap show _rows
      showColumns = "columns:\n" ++ concatMap show _cols
      width = length _cols
      height = length _rows

type Spec = ([[Int]], [[Int]])

