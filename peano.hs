data Peano = Zero | Succ Peano

inc :: Peano -> Peano
inc x = Succ (x)

dec :: Peano -> Peano
dec (Succ x) = x

add :: Peano -> Peano -> Peano
add Zero y = y
add (Succ x) = add x (Succ y)
