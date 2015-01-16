module Model.Row (
    Row(..)
) where

data Row = Row {
    space :: Int
  , combinations :: [[Int]]
  , blocks :: [Int]
} deriving Show


