data Board = Board {
    boardRows :: [Row]
  , boardColumns :: [Row]
}

instance Show Board where
  show (Board [] []) = ""
  show (Board [] (c:cs)) = show c ++ "\n" ++ show (Board [] cs)
  show (Board (r:rs) cs) = show r ++ "\n" ++ show (Board rs cs)

data Row = Row {
    rowBlocks :: [Int]
  , rowCells :: [Cell]
}

instance Show Row where
  show (Row blocks cells) = show cells ++ " " ++ show blocks

data Cell = Filled | Blank | Unknown

instance Show Cell where
  show Filled = "X"
  show Blank = "."
  show Unknown = "_"

type BoardSpec = ([[Int]], [[Int]])

exampleSpec :: BoardSpec
exampleSpec = ([
    [3,5,3], [2,5,2], [1,1,3,1,1], [1,7,1], [3,1,3],
    [3,1,2], [2,1,2], [2,3,2], [2,2], [3,2],
    [3,3], [1,7,1], [1,1,3,1,1], [2,5,2], [3,5,3]
  ],[
    [4,4], [2,2], [1,5,1], [7], [4,4],
    [2,2,2,2], [4,4], [8,4], [4,1,4], [2,2,1,2,2],
    [4,4], [7], [1,3,1], [2,2], [4,4]
  ])

makeBoard :: BoardSpec -> Board
makeBoard (rows,columns) =
    Board {
        boardRows = map (makeRow width) rows
      , boardColumns = map (makeRow height) columns
    }
  where
    width = length columns
    height = length rows

    makeRow :: Int -> [Int] -> Row
    makeRow size blocks = Row {
        rowBlocks = blocks
      , rowCells = replicate size Unknown
    }

main :: IO ()
main = print $ makeBoard exampleSpec
