import Tree

toList :: RAList a -> [a]
toList = concatMap to
 where
  to :: Maybe (Tree a) -> [a]
  to Nothing = []
  to (Just tr) = flatten tr
