module Solver (
    solve
) where

import Data.List (transpose)

import Combinator
import Model.Cell
import Model.Puzzle

solve :: Puzzle -> Int -> Puzzle
solve puzzle steps =
    foldl (\value f -> f value) puzzle belt
  where
    (hcs, vcs) = calculateCombinations puzzle
    belt = take steps $ cycle [solveHorizontal hcs, solveVertical vcs]

solveVertical :: [[[Cell]]] -> Puzzle -> Puzzle
solveVertical combs puzzle = puzzle {cellMatrix = transpose evaluated}
  where
    evaluated = zipWith evaluate combs (transpose $ cellMatrix puzzle)

solveHorizontal :: [[[Cell]]] -> Puzzle -> Puzzle
solveHorizontal combs puzzle = puzzle {cellMatrix = evaluated}
  where
    evaluated = zipWith evaluate combs (cellMatrix puzzle)

evaluate :: [[Cell]] -> [Cell] -> [Cell]
evaluate cellConfigs currentConfig =
    map compressConfig (transpose matchingConfigs)
  where
    matchingConfigs = (filter $ matchesConfig currentConfig) cellConfigs

matchesConfig :: [Cell] -> [Cell] -> Bool
matchesConfig [] _ = True
matchesConfig (Filled:_) (Blank:_) = False
matchesConfig (Blank:_) (Filled:_) = False
matchesConfig (_:ss) (_:cs) = matchesConfig ss cs

compressConfig :: [Cell] -> Cell
compressConfig (Filled:xs) | Blank `elem` xs = Unknown
                           | otherwise = Filled
compressConfig (Blank:xs)  | Filled `elem` xs = Unknown
                           | otherwise = Blank
