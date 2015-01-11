module Types.Row (Row(..)) where

import Types.Block
import Types.Cell

data Row = Row {
    blocks :: [Block]
  , cells :: [Cell]
  , margin :: Int
  , hasOverlap :: Bool
}

spaceTen :: Int -> String
spaceTen x | x < 10 = " " ++ show x
           | otherwise = show x

instance Show Row where
  show row =
    show (cells row) ++ " " ++
    spaceTen (margin row) ++ (if hasOverlap row then "*" else " ") ++
    " " ++ show (blocks row) ++ "\n"

