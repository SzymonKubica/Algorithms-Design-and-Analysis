import qualified Tree
import Prelude hiding ((!!))

type RAList a = [Maybe (Tree.Tree a)]

flatten :: Tree.Tree a -> [a]
flatten (Tree.Leaf x) = [x]
flatten (Tree.Node _ lt rt) = flatten lt ++ flatten rt

toList :: RAList a -> [a]
toList = concatMap to
 where
  to :: Maybe (Tree.Tree a) -> [a]
  to Nothing = []
  to (Just tr) = flatten tr

(!!) :: RAList a -> Int -> a
(Nothing : ts) !! k = ts !! k
(Just t : ts) !! k
  | k < m = t Tree.!! k
  | otherwise = ts !! (k - m)
 where
  m = Tree.size t

cons :: a -> RAList a -> RAList a
cons x = consT (Tree.Leaf x)
 where
  consT :: Tree.Tree a -> RAList a -> RAList a
  consT t [] = [Just t]
  consT t (Nothing : ts) = Just t : ts
  consT t ((Just t') : ts) = Nothing : consT (Tree.node t t') ts
