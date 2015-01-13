module Main where

import Control.Monad (replicateM)
import Data.Function.Memoize (memoize)
import Data.List (transpose)
import System.Environment (getArgs)

data Puzzle = Puzzle {
    horizontalBlocks :: [[Int]]
  , verticalBlocks :: [[Int]]
  , cellMatrix :: [[Cell]]
} deriving Show

data Cell = Unknown | Filled | Blank deriving Eq

instance Show Cell where
    show Unknown = " "
    show Filled = "x"
    show Blank = "-"

solve :: Puzzle -> Int -> Puzzle
solve puzzle steps =
    foldl (\value f -> f value) puzzle belt
  where belt = take steps $ cycle [solveHorizontal, solveVertical]

solveVertical :: Puzzle -> Puzzle
solveVertical puzzle = puzzle {cellMatrix = transpose evaluated}
  where
    evaluated = zipWith (evaluate size) (transpose $ cellMatrix puzzle) (verticalBlocks puzzle)
    size = length (horizontalBlocks puzzle)

solveHorizontal :: Puzzle -> Puzzle
solveHorizontal puzzle = puzzle {cellMatrix = evaluated}
  where
    evaluated = zipWith (evaluate size) (cellMatrix puzzle) (horizontalBlocks puzzle)
    size = length (verticalBlocks puzzle)

evaluate :: Int -> [Cell] -> [Int] -> [Cell]
evaluate size config =
    map compressConfig . transpose . filter (hasKnownConfig config) . allCellConfigs size

hasKnownConfig :: [Cell] -> [Cell] -> Bool
hasKnownConfig [] _ = True
hasKnownConfig (Filled:_) (Blank:_) = False
hasKnownConfig (Blank:_) (Filled:_) = False
hasKnownConfig (_:ss) (_:cs) = hasKnownConfig ss cs

allCellConfigs :: Int -> [Int] -> [[Cell]]
allCellConfigs size = memoize $ map interpretConfig . configVariations size

compressConfig :: [Cell] -> Cell
compressConfig (Filled:xs) | Blank `elem` xs = Unknown
                           | otherwise = Filled
compressConfig (Blank:xs) | Filled `elem` xs = Unknown
                          | otherwise = Blank

interpretConfig :: [Int] -> [Cell]
interpretConfig [x] = replicate x Blank
interpretConfig (x:y:xs) =
    replicate x Blank ++ replicate y Filled ++ interpretConfig xs

configVariations :: Int -> [Int] -> [[Int]]
configVariations space blocks =
    map (mergeBlocks blocks) $ gapVariations space blocks 

mergeBlocks :: [Int] -> [Int] -> [Int]
mergeBlocks [] [gap] = [gap]
mergeBlocks (block:blocks) (gap:gaps) =
    gap : block : mergeBlocks blocks gaps

gapMinLengths :: [Int]-> [Int]
gapMinLengths blocks = 0 : replicate (length blocks - 1) 1 ++ [0]

gapVariations :: Int -> [Int] -> [[Int]]
gapVariations space blocks =
    addMinLengths $ filter (\x -> sum x == v) permutations
  where
    v = variability space blocks
    permutations = replicateM (length blocks + 1) [0..v]
    addMinLengths = map . zipWith (+) $ gapMinLengths blocks

variability :: Int -> [Int] -> Int
variability space blocks = space - sum blocks - length blocks + 1

type PuzzleSpec = ([[Int]],[[Int]]) 

exampleConfig :: PuzzleSpec
exampleConfig = ([
      [3,5,3], [2,5,2], [1,1,3,1,1], [1,7,1], [3,1,3],
      [3,1,2], [2,1,2], [2,3,2], [2,2], [3,2],
      [3,3], [1,7,1], [1,1,3,1,1], [2,5,2], [3,5,3]
    ], [
      [4,4], [2,2], [1,5,1], [7], [4,4],
      [2,2,2,2], [4,4], [8,4], [4,1,4], [2,2,1,2,2],
      [4,4], [7], [1,3,1], [2,2], [4,4]
    ])

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

main :: IO ()
main = do
    args <- getArgs
    content <- readFile ("puzzles/" ++ head args)
    drawPuzzle $ solve (parsePuzzle content) (read $ args !! 1)
    
