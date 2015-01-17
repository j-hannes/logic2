module Main (main) where

import System.Environment (getArgs)

import Controller.Puzzle (parsePuzzle)
import Solver

import Model.PuzzlePlus

loadPuzzle :: Int -> IO PuzzlePlus
loadPuzzle n = do
    content <- readFile $ "puzzles/" ++ show n
    return $ parsePuzzle content

-- | Usage: ./blocks [puzzleNumber] [stepsOfSolution]
main :: IO ()
main = do
    -- receive and parse command line arguments
    [puzzleNumber, steps] <- getArgs

    -- read the puzzle config from file system and parse into puzzle
    puzzle <- loadPuzzle (read puzzleNumber)
    
    -- precalculate all block combinations of the puzzle
    let solvedPuzzle = solve puzzle (read steps)

    -- output the (partially) solved puzzle
    print solvedPuzzle
 
