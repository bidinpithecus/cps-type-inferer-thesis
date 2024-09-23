addCps :: Int -> Int -> (Int -> r) -> r
addCps x y k = k (x + y)