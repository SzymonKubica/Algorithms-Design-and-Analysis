-- Impelmentation of Eq class in haskell.
-- class Eq a where
--  (==) :: a -> a -> Bool

rummage :: Eq a => a -> [a] -> Bool
rummage x [] = False
rummage x (y : ys)
  | x == y = True
  | otherwise = rummage x ys

-- In order to improve the complexity of searching we need to be able to impose
-- an order on the elements of the list that we want to search through.

-- Partial Order in haskell
-- class Eq a => Ord a where
--   (<=) :: a -> a -> Bool
--   (<) :: a -> a -> Bool
--   (>=) :: a -> a -> Bool
--   (>) :: a -> a -> Bool

class OrdList ordlist where
  empty :: ordlist a
  insert :: Ord a => a -> ordlist a -> ordlist a
  merge :: Ord a => ordlist a -> ordlist a -> ordlist a
  member :: Ord a => a -> ordlist a -> Bool
  fromOrdlist :: Ord a => ordlist a -> [a]
  toOrdlist :: Ord a => [a] -> ordlist a

data Tree a = Tip | Node (Tree a) a (Tree a)
  deriving (Show)

instance OrdList Tree where
  empty = Tip

  insert x Tip = Node Tip x Tip
  insert x (Node lt y rt)
    | x == y = Node lt y (Node Tip x rt)
    | x < y = Node (insert x lt) y rt
    | otherwise = Node lt y (insert x rt)

  member x Tip = False
  member x (Node lt y rt)
    | x == y = True
    | x < y = member x lt
    | otherwise = member x rt
