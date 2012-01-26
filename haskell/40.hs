isPrime :: Int -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n = and [n `mod` s /= 0 | s <- [2..n-1]]

primeR :: Int -> Int -> [Int]
primeR a b = filter isPrime [a..b]

goldbach :: Int -> (Int, Int)
goldbach n = head $ filter (\(a, b) -> isPrime b) $ map (\x -> (x, n-x)) $ primeR 2 n