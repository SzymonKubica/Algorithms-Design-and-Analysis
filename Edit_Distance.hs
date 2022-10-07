import Data.Array

edistance :: String -> String -> Int
edistance [] [] = 0
edistance xs [] = length xs
edistance [] ys = length ys
edistance str1@(x : xs) str2@(y : ys) = minimum [left, center, right]
 where
  left = 1 + edistance xs str2
  center
    | x == y = edistance xs ys
    | otherwise = 1 + edistance xs ys
  right = 1 + edistance str1 ys

edistance' :: String -> String -> Int -> Int -> Int
edistance' xs ys 0 j = j
edistance' xs ys i 0 = i
edistance' xs ys i j =
  minimum
    [ 1 + edistance' xs ys (i + 1) j
    , 1 + edistance' xs ys i (j + 1)
    , edistance' xs ys (i + 1) (j + 1) + if x == y then 0 else 1
    ]
 where
  x = xs !! i
  y = ys !! j

edistance'' :: String -> String -> Int
edistance'' str1 str2 = table ! (n, m)
 where
  table = tabulate ((0, 0), (n, m)) memo
  n = length str1
  m = length str2
  memo (0, j) = j
  memo (i, 0) = i
  memo (i, j) =
    minimum
      [ 1 + table ! (i - 1, j)
      , 1 + table ! (i, j - 1)
      , table ! (i - 1, j - 1) + if x == y then 0 else 1
      ]
   where
    x = str1 !! (n - i)
    y = str2 !! (m - j)

tabulate :: (Ix a) => (a, a) -> (a -> b) -> Array a b
tabulate bounds f = array bounds [(i, f i) | i <- range bounds]
