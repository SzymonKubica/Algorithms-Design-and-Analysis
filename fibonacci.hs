import Data.Array

fib :: Int -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fib' :: Int -> Integer
fib' 0 = 1
fib' 1 = 0
fib' n = table ! n
 where
  table = tabulate (0, n) memo
   where
    memo 0 = 1
    memo 1 = 1
    memo n = table ! (n - 1) + table ! (n - 2)

tabulate :: (Ix a) => (a, a) -> (a -> b) -> Array a b
tabulate bounds f = array bounds [(i, f i) | i <- range bounds]
