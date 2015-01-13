module Testing.ExamplePuzzleSpec (exampleConfig) where
import Model.PuzzleSpec
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
