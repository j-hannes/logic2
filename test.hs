module Main where

import Control.Monad

-- import Control.Monad (replicateM)
-- import Data.List (nub)

main :: IO ()
main = undefined

-- createCombinations n size = 

--mapOnEach :: 
--mapOnEach f s xs = take (s-1) xs ++ [f $ xs !! (s-1)] ++ drop s xs

-- incrementEach' :: [Int] -> Int -> [Int]
-- incrementEach' xs s = take (s-1) xs ++ [xs !! (s-1) + 1] ++ drop s xs

-- incrementEach :: Int -> [Int] -> [Int]
-- incrementEach s xs = take (s-1) xs ++ [xs !! (s-1) + 1] ++ drop s xs

-- magic :: Int -> [Int] -> [[Int]]
-- magic n xs = zipWith incrementEach [n .. length xs] (replicate (1 + length xs - n) xs)
--magic xs = map (\x -> incrementEach [1 .. length xs] xs) [1 .. length xs]

fill n xs = xs ++ [n - sum xs]

--gapCombinations variation gaps =
    -- {-nub $-} map (fill variation) $ gapLenghts : (f 1) variation gapLenghts
  --where
    -- gapLenghts = replicate (gaps - 1) 0

--f a 0 xs = [xs]
--f a n xs = magic a xs ++ concatMap (magic (a+1)) (f a (n-1) xs)

--awesome variation gaps = map ( f (variation

--produce n m = [m..n]

e3 variation fields = map (fill variation) $ filter (\xs -> sum xs <= variation) $ replicateM (fields-1) [0..variation]

--data RoseTree a = RoseTree {
    --transformation :: a -> [a]
  --, nextLevel :: [RoseTree a]
  --, node :: a
--}

-- instance Functor RoseTree where
  -- fmap f (RoseTree t n x) = RoseTree t (map f n) x 

--instance (Show a) => Show (RoseTree a) where
  --show (RoseTree _ [] a) = show a
  --show (RoseTree _ nl a) = show a ++ " " ++ show nl


--extend (RoseTree f n x) = RoseTree f (map (RoseTree f []) (f x)) x

-- buildTreeWith 0 _ _ = []
-- buildTreeWith d f x = RoseTree x (map flip Rpse



data RoseTree a = RoseTree a [RoseTree a] deriving Show

newRoseTree x = RoseTree x []

nextLevel (RoseTree s _) = RoseTree s (map newRoseTree $ ff s)

roseTreeUntil 0 f x = RoseTree x []
roseTreeUntil n f x =
  RoseTree x (map (roseTreeUntil (n-1) f) (f x))



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

s1, s2, s3 :: (Int, [Int])
s1 = (1,[0])
s2 = (2,[0,0])
s3 = (3,[0,0,0])

buildRoseTree variation gaps = roseTreeUntil variation ff (gaps, replicate gaps 0)

collectFromTree (RoseTree x []) = [x]
collectFromTree (RoseTree x xs) = x : concatMap collectFromTree xs

e4 variation gaps = map (fill variation) $ map snd $ collectFromTree $ buildRoseTree (variation ) (gaps - 1)
