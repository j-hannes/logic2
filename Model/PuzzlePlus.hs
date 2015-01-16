module Model.PuzzlePlus (
    PuzzlePlus(..)
  , parsePuzzlePlus
  , rowQueue
) where

import Data.List (sortBy)

import Model.Cell
import Model.Row
import Controller.Puzzle(parseConfig)

data PuzzlePlus = PuzzlePlus {
    horizontalRows :: [Row]
  , verticalRows :: [Row]
  , cellMatrix :: [[Cell]]
} deriving Show

-- | Check if a puzzle has a valid setting.
--
-- Let w be the width of the puzzle and h be the height of the puzzle,
-- then all horizontal rows must have space w, all vertical rows must have
-- space h and the length of cells must be w*h.
--
isValid :: PuzzlePlus -> Bool
isValid (PuzzlePlus hr vr cm) =
    let dimension = length hr * length vr 
    in sum (map space hr) == dimension &&
       sum (map space vr) == dimension &&
       length cm * length (head cm) == dimension

-- | Generate a new puzzle from a given configuration.
--
-- If the configuration doesn't match then 
generatePuzzle :: (Int,Int) -> [[Int]] -> [[Int]] -> Maybe PuzzlePlus
generatePuzzle (width, height) horizontalBlocks verticalBlocks
  | isValid puzzle = Just puzzle
  | otherwise = Nothing
  where
    puzzle = PuzzlePlus hRows vRows cells
    hRows = map (Row width) horizontalBlocks
    vRows = map (Row height) verticalBlocks
    cells = replicate height $ replicate width Unknown

parsePuzzlePlus :: String -> Maybe PuzzlePlus
parsePuzzlePlus inputStream =
    generatePuzzle (length vBlocks, length hBlocks) hBlocks vBlocks
  where
    (hBlocks, vBlocks) = parseConfig inputStream

-- data EvalPuzzle = EvalPuzzle {
    -- puzzle :: PuzzlePlus
  -- , queue :: (Int, Int)
-- } deriving Show

rowQueue :: PuzzlePlus -> [(Int, Int)]
rowQueue (PuzzlePlus hRows vRows _) = sortBy (flip compare) $ hVars ++ vVars
  where
    hVars = zip (map overlap hRows) [1..]
    vVars = zip (map overlap vRows) [n..]
    n = length hRows + 1

overlap :: Row -> Int
overlap row@(Row _ bs) = maximum bs - variability row

variability :: Row -> Int
variability (Row s bs) = s - sum bs - length bs + 1

