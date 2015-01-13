module Model.Row (
    Row(..)
) where

data Row = Row {
    blocks :: [Int]
  , space :: Int
} deriving Show


