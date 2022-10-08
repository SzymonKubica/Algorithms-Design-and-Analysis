module Tree where

import Prelude hiding ((!!))

data Tree a = Leaf a | Node Int (Tree a) (Tree a)
  deriving (Show)

size :: Tree a -> Int
size (Leaf x) = 1
size (Node x lt rt) = x

-- We can use a smart constructor 'node' which ensures that nodes always have
-- the correct size on them (preserves the size invariance)

node :: Tree a -> Tree a -> Tree a
node lt rt = Node (size lt + size rt) lt rt

(!!) :: Tree a -> Int -> a
(Leaf x) !! 0 = x
(Node s lt rt) !! k
  | k < size lt = lt !! k
  | k >= size lt = rt !! (k - size lt)

flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node _ lt rt) = flatten lt ++ flatten rt
