module Types.Board (
    Board
  , Grid(..)
  , draw
  , Spec
  ) where

import Types.Row

data Grid a = Grid {
    rows :: [a]
  , columns :: [a]
}

instance Functor Grid where
  fmap f (Grid rs cs) = Grid (map f rs) (map f cs)

type Board = Grid Row

draw :: Board -> String
draw board = showTitle ++ showRows ++ showColumns
    where
      showTitle = "BOARD (" ++ show width ++ "x" ++ show height ++ ")\n"
      showRows = "rows:\n" ++ concatMap show (rows board)
      showColumns = "columns:\n" ++ concatMap show (columns board)
      width = length $ columns board
      height = length $ rows board

type Spec = ([[Int]], [[Int]])

