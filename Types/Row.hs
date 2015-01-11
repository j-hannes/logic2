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
  show (Row _blocks _cells _margin _hasOverlap) =
    show _cells ++ " " ++ spaceTen _margin ++ 
    (if _hasOverlap then "*" else " ") ++ " " ++ show _blocks ++ "\n"

