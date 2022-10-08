module Lists where

newtype DList a = DList ([a] -> [a])

instance Show a => Show (DList a) where
  show (DList fxs) = show (fxs [])

instance List DList where
  toList :: DList a -> [a]
  toList (DList fxs) = fxs []
  fromList :: [a] -> DList a
  fromList xs = DList (xs Prelude.++)

concatD :: DList a -> DList a -> DList a
concatD (DList fxs) (DList fys) = DList (fxs . fys)

concat' :: [[a]] -> [a]
concat' xss = toList (foldr concatD (DList id) (map fromList xss))

class List list where
  empty :: list a
  cons :: a -> list a -> list a
  snoc :: list a -> a -> list a
  head :: list a -> a
  tail :: list a -> list a
  (++) :: list a -> list a -> list a
  toList :: list a -> [a]
  fromList :: [a] -> list a
