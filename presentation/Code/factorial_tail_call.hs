go :: Int -> Int -> Int
go 1 a = a
go n a = go (n - 1) (a * n)

factorial :: Int -> Int
factorial 0 = 1
factorial n = go n 1
