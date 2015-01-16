module Permutator (
    e3
  , e4
) where

import Control.Monad (replicateM)

fill :: Int -> [Int] -> [Int]
fill n xs = xs ++ [n - sum xs]

e3 :: Int -> Int -> [[Int]]
e3 variation gapCount =
    map (fill variation)
      $ filter (\xs -> sum xs <= variation)
      $ replicateM (gapCount-1) [0..variation]


-- | Increases the value of a specific position of a list by one.
increase :: Int -> [Int] -> [Int]
increase s xs = take s xs ++ [xs !! s + 1] ++ drop (s+1) xs

-- | Takes a 
applyIncrease :: (Int, [Int]) -> (Int, [Int])
applyIncrease (p,l) = (p, increase (length l - p) l)

-- | Creates new tree branches according to a tuple that contains
-- an increase value i and a list of values to be incrememnted.
-- For example let s0 be (3,[0,0,0]), then ff s0 will be
-- [(3,[1,0,0]),(2,[0,1,0]), (1,[0,0,1])].
--
-- Initial implementation
-- ff (3,[a,b,c]) = [(3,[a+1,b,c]),(2,[a,b+1,c]),(1,[a,b,c+1])]
-- ff (2,[a,b,c]) = [(2,[a,b+1,c]),(1,[a,b,c+1])]
-- ff (1,[a,b,c]) = [(1,[a,b,c+1])]
ff :: (Int, [Int]) -> [(Int, [Int])]
ff (1,xs) = [applyIncrease (1,xs)]
ff (n,xs) = applyIncrease (n,xs) : ff (n-1,xs)

data RoseTree a = RoseTree a [RoseTree a]

type Node = (Int,[Int])

roseTreeUntil :: Int -> (Node -> [Node]) -> Node -> RoseTree Node
roseTreeUntil 0 _ x = RoseTree x []
roseTreeUntil n f x =
  RoseTree x (map (roseTreeUntil (n-1) f) (f x))

buildRoseTree :: Int -> Int -> RoseTree Node
buildRoseTree variation gaps =
    roseTreeUntil variation ff (gaps, replicate gaps 0)

collectFromTree :: RoseTree Node -> [Node]
collectFromTree (RoseTree x []) = [x]
collectFromTree (RoseTree x xs) = x : concatMap collectFromTree xs

e4 :: Int -> Int -> [[Int]]
e4 variation gaps =
    map (fill variation . snd) (collectFromTree tree)
  where
    tree = buildRoseTree variation (gaps - 1)

