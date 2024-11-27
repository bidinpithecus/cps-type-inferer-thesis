fatCps :: Int -> (Int -> r) -> r
fatCps 0 k = k 1
fatCps n k =
  fatCps
    (n - 1)
    (\x -> seq x k (n * x))