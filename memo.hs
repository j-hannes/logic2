module Memo where

import Data.Function.Memoize

fib = memoize fib'

fib' :: Integer -> Integer
fib' 1 = 1
fib' 2 = 1
fib' n = fib (n-1) + fib (n-2)
