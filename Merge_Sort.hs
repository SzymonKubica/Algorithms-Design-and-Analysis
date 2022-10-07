import Prelude hiding (splitAt)

msort :: [Int] -> [Int]
msort [] = []
msort [x] = [x]
msort xs = merge (msort us) (msort vs)
 where
  (us, vs) = splitAt (length xs `div` 2) xs

merge :: [Int] -> [Int] -> [Int]
merge xs [] = xs
merge [] ys = ys
merge list1@(x : xs) list2@(y : ys)
  | x < y = x : merge xs list2
  | otherwise = y : merge list1 ys

splitAt :: Int -> [Int] -> ([Int], [Int])
splitAt n list@(x : xs)
  | n == 0 = ([], list)
  | n >= length list = (list, [])
  | otherwise = (x : us, vs)
 where
  (us, vs) = splitAt (n - 1) xs
