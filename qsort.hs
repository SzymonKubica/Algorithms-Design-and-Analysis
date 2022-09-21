qsort :: [Int] -> [Int]
qsort [] = []
qsort [x] = [x]
qsort (x : xs) = qsort us ++ (x : qsort vs)
 where
  (us, vs) = partition (<= x) xs

split :: Int -> [Int] -> ([Int], [Int])
split pivot [] = ([], [])
split pivot (x : xs)
  | x <= pivot = (x : us, vs)
  | otherwise = (us, x : vs)
 where
  (us, vs) = split pivot xs

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p xs = (filter p xs, filter (not . p) xs)
