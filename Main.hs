module Main (main) where

import Types.Board (Board)
import Types.Row (Row)
import Types.Cell (Cell(..))
import qualified Types.Row as Row
import qualified Types.Block as Block

import Data.List (intersperse)

import qualified Types.Board as Board
import BoardGenerator (makeBoard)

exampleSpec :: Board.Spec
exampleSpec = ([[2],[2,2],[4],[2],[1]],[[3],[4],[1],[3],[2]])
    {-
    [3,5,3], [2,5,2], [1,1,3,1,1], [1,7,1], [3,1,3],
    [3,1,2], [2,1,2], [2,3,2], [2,2], [3,2],
    [3,3], [1,7,1], [1,1,3,1,1], [2,5,2], [3,5,3]
  ],[
    [4,4], [2,2], [1,5,1], [7], [4,4],
    [2,2,2,2], [4,4], [8,4], [4,1,4], [2,2,1,2,2],
    [4,4], [7], [1,3,1], [2,2], [4,4]
  ])
  -}

-- [-,-,x,x,x,-,x,x,x,x,x,-,x,x,x]
-- [x,x,x,-,-,-,x,x,x,x,x,-,x,x,x]

board :: Board
board = makeBoard exampleSpec

firstRow :: Row
firstRow = head $ Board.rows board


-- row length: 15
-- blocks given: 3, 5, 3
-- ==> rowLength = 15
-- ==> blockLenghts = [3,5,3]
-- 
-- => sum of blockLengths is 3 + 5 + 3 = 11
-- between each block is at least one free cell
-- => space required is 11 + 2 = 13
-- ==> spaceRequired xs = sum xs + length xs - 1
--
-- means each block can shift by 2 fields
-- => the two 3-blocks have 1 field always filled
-- => the 5-block has 3 fields always filled
-- => mandatory fields have lengths 1, 3, 1
-- ==> mandatoryFields margin = map (\x -> margin - x)
  -- [1,3,1] 
--
-- blocks stepwise are 3; 3 and 5; 3, 5 and3
-- ==> stepwise = filter (not.null) . tails
-- ==> beginnings = map ((-) 16 ) 
-- 
-- positions of the fields is buffer + 1 + previous lengths (incl buffer)
-- => positions are 2 + 1 + 0 = 3, 2 + 1 + 3 + 1 = 7, 2 + 1 + 8 + 2 = 13
-- ==>
--

evaluate1 :: Row -> Row
evaluate1 row = row { Row.cells = evaluated }
  where
    evaluated = map translateToCell overlap
    overlap = zip confL confR
    confR = extend rowLength conf
    confL = reverse $ extend rowLength $ reverse conf
    conf = rowToConf row
    rowLength = length $ Row.cells row

translateToCell :: (Int, Int) -> Cell
translateToCell (0,0) = Blank
translateToCell (a,b) | a == b = Filled
                | otherwise = Unknown

extend :: Int -> [Int] -> [Int]
extend n list | n <= length list = list
              | otherwise = 0 : extend (n-1) list

rowToConf :: Row -> [Int]
rowToConf = makeConf . replicateIdentifier . zipWithIds . getBlockValues
              
makeConf :: [[Int]] -> [Int]
makeConf = concat . intersperse [0]

replicateIdentifier :: [(Int, Int)] -> [[Int]]
replicateIdentifier = map (\(i, l) -> replicate l i)

zipWithIds :: [Int] -> [(Int, Int)]
zipWithIds = zip [1..]

getBlockValues :: Row -> [Int]
getBlockValues = map Block.value . Row.blocks



{-
  where
    sideBuffer = replicate margin Unknown
    margin = Row.margin row
    overlapArea = concat . intersperse inbetweenBuffer $ overlapBlocks
    inbetweenBuffer = Unknown : sideBuffer
    overlapBlocks = map (flip replicate Filled) overlapLengths
    overlapLengths = map (\b -> Block.value b - margin) $ Row.blocks row
    -}


main :: IO ()
main = putStrLn $ Board.draw board
