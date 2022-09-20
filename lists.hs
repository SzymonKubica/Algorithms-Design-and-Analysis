module Lists where

data DList a = DList ([a] -> [a])

instance Show a => Show (DList a) where
  show (DList fxs) = show (fxs [])

toList:: DList a -> [a]
toList (DList fxs) = fxs []

fromList:: [a] -> DList a
fromList xs = DList (xs++)

concatD :: DList a -> DList a -> DList a
concatD (DList fxs) (DList fys) = DList (fxs . fys)

concat' :: [[a]] -> [a]
concat' xss = toList (foldr concatD (DList id) (map fromList xss))

class List a where
  empty :: List a
  cons  :: a -> List a -> List a
  snoc  :: List a -> a -> List a
  head  :: List a -> a
  tail  :: List a -> List a
  (++)  :: List a -> List a -> List a
  toList :: List a -> [a]
  fromList :: [a] -> List a

