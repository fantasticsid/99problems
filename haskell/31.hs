isPrime :: Int -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n = and [n `mod` s /= 0 | s <- [2..n-1]]
