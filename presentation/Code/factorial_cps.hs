subCps :: Int -> Int -> (Int -> r) -> r
subCps x y k = k (x - y)

multCps :: Int -> Int -> (Int -> r) -> r
multCps x y k = k (x * y)

factorialCps :: Int -> (Int -> r) -> r
factorialCps 0 k = k (1)
factorialCps n k = 
  subCps n 1 (\nMinus1 ->
    factorialCps nMinus1 (\factNMinus1 ->
      multCps n factNMinus1 k))
