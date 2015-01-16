module Main (main) where

import Data.Function.Memoize (memoize)
import System.Environment (getArgs)

import Controller.Puzzle
import Model.BlockLenghts
import Solver

import Model.Puzzle

loadPuzzle n = do
    content <- readFile $ "puzzles/" ++ show n
    return $ parsePuzzle content

-- | Usage: ./blocks [puzzleNumber] [stepsOfSolution]
main :: IO ()
main = do
    -- receive and parse command line arguments
    [arg1, arg2] <- getArgs
    let puzzleNumber = arg1
        steps = read arg2 :: Int

    -- read the puzzle config from file system and parse into puzzle
    fileContent <- readFile $ "puzzles/" ++ puzzleNumber
    let puzzle = parsePuzzle fileContent
    
    -- precalculate all block combinations of the puzzle
    let solvedPuzzle = solve puzzle steps

    -- output the (partially) solved puzzle
    drawPretty solvedPuzzle
 
