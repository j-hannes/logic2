module Types.Cell (Cell(..)) where

data Cell = Filled | Blank | Unknown

instance Show Cell where
  show Filled = "x"
  show Blank = "."
  show Unknown = "_"

