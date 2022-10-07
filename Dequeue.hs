data Dequeue a = Dequeue [a] [a]

toList :: Dequeue a -> [a]
toList (Dequeue us sv) = us ++ reverse sv

fromList :: [a] -> Dequeue a
fromList xs = Dequeue us sv
 where
  (us, vs) = splitAt (n `div` 2) xs
  sv = reverse vs
  n = length xs

cons :: a -> Dequeue a -> Dequeue a
cons x (Dequeue us sv) = Dequeue (x : us) sv

snoc :: Dequeue a -> a -> Dequeue a
snoc (Dequeue [] sv) x = Dequeue sv [x]
snoc (Dequeue us sv) x = Dequeue us (x : sv)

last :: Dequeue a -> a
last (Dequeue us []) = head us
last (Dequeue us (v : sv)) = v

tail :: Dequeue a -> Dequeue a
tail (Dequeue [] sv) = Dequeue [] (Prelude.tail sv)
tail (Dequeue [x] sy) = Dequeue us sv
 where
  ys = reverse sy
  (us, vs) = splitAt (length ys `div` 2) ys
  sv = reverse vs
tail (Dequeue xs sy) = Dequeue (Prelude.tail xs) sy

instance Show a => Show (Dequeue a) where show deq = Prelude.show (toList deq)
