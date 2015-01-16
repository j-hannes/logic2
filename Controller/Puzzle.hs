module Controller.Puzzle (
    makePuzzle
  , drawPuzzle
  , drawPretty
  , parsePuzzle
  , parseConfig
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

drawPretty :: Puzzle -> IO ()
drawPretty (Puzzle _ _ cm) = do
  drawLine (replicate (length (head cm)) Blank)
  mapM_ drawLine cm
  drawLine (replicate (length (head cm)) Blank)
  putStrLn ""

drawLine :: [Cell] -> IO ()
drawLine cl = do
  putStr $ coloredFields Blank
  putStr (concatMap coloredFields cl)
  putStrLn $ coloredFields Blank

coloredFields :: Cell -> String
coloredFields Blank = show White
coloredFields Filled = show Black
coloredFields Unknown = show Green --"\ESC[1;90m[]\ESC[m"

data ColorBlock =
    Black | White | Cyan | Purple | Blue | Yellow | Green | Red
  deriving (Enum, Eq)

instance Show ColorBlock where
    show Black = "  "
    show White = "\ESC[0;47m  \ESC[m"
    show Cyan = "\ESC[0;46m  \ESC[m"
    show Purple = "\ESC[0;45m  \ESC[m"
    show Blue = "\ESC[0;44m  \ESC[m"
    show Yellow = "\ESC[0;43m  \ESC[m"
    show Green = "\ESC[0;42m  \ESC[m"
    show Red = "\ESC[0;41m  \ESC[m"

parsePuzzle :: String -> Puzzle
parsePuzzle = makePuzzle . parseConfig

parseConfig :: String -> ([[Int]], [[Int]])
parseConfig inputStream = (readValues hBlocks, readValues vBlocks)
  where 
    readValues = map (map read . words)
    hBlocks = takeWhile (not.null) $ lines inputStream
    vBlocks = tail . dropWhile (not.null) $ lines inputStream


