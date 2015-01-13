module Controller.Puzzle (
    makePuzzle
  , drawPuzzle
  , parsePuzzle
) where

import Model.Puzzle (Puzzle(..))
import Model.PuzzleSpec
import Model.Cell

makePuzzle :: PuzzleSpec -> Puzzle
makePuzzle (hBlocks,vBlocks) = Puzzle hBlocks vBlocks matrix
  where
    matrix = replicate height $ replicate width Unknown
    height = length vBlocks
    width = length hBlocks

drawPuzzle :: Puzzle -> IO ()
drawPuzzle puzzle = mapM_ print $ cellMatrix puzzle

parsePuzzle :: String -> Puzzle
parsePuzzle content = 
    let hBlocks = takeWhile (not.null) $ lines content
        vBlocks = tail . dropWhile (not.null) $ lines content
    in makePuzzle (map (map read . words) hBlocks,
                        map (map read . words) vBlocks)

