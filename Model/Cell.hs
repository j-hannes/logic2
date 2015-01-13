module Model.Cell (
    Cell(..)
) where

data Cell = Unknown | Filled | Blank deriving Eq

instance Show Cell where
    show Unknown = " "
    show Filled = "x"
    show Blank = "-"

