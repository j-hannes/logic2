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
  show board = showTitle ++ showRows ++ showColumns
    where
      showTitle = "BOARD (" ++ show width ++ "x" ++ show height ++ ")\n"
      showRows = "rows:\n" ++ concatMap show (rows board)
      showColumns = "columns:\n" ++ concatMap show (columns board)
      width = length $ columns board
      height = length $ rows board

type Spec = ([[Int]], [[Int]])

