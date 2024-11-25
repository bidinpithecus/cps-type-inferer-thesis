sub :: Int -> Int -> Int
sub x y = x - y

mult :: Int -> Int -> Int
mult x y = x * y

factorial :: Int -> Int
factorial 0 = 1
factorial n = mult n (factorial (sub n 1))
