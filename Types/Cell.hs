module Types.Cell (Cell(..)) where

data Cell = Filled | Blank | Unknown

instance Show Cell where
  show Filled = "X"
  show Blank = "."
  show Unknown = "_"

