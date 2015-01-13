module Types.Block (Block(..)) where

data Block = Block {
    value :: Int
  , hasOverlap :: Bool
}

instance Show Block where
  show (Block size True)  = show size ++ "*"
  show (Block size False) = show size ++ " "

