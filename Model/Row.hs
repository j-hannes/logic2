module Model.Row (
    Row(..)
) where

data Row = Row {
    space :: Int
  , blocks :: [Int]
} deriving Show


