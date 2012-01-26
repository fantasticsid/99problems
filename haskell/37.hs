encode :: (Eq a) => [a] -> [(Int, a)]
encode (x:[]) = [(1, x)]
encode (x:xs) = if x == head xs
                then (1 + fst (head (encode xs)), x):(tail (encode xs))
                else  (1, x):(encode xs)

isPrime :: Int -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n = and [n `mod` s /= 0 | s <- [2..n-1]]

coprime :: Int -> Int -> Bool
coprime a b = gcd a b == 1

primeFactors :: Int -> [Int]
primeFactors 1 = []
primeFactors n = let m = head $ filter (\x -> isPrime x && n `mod` x == 0) [2..n]
                 in m:(primeFactors $ div n m)

primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult = map (\(a, b) -> (b, a)) . encode . primeFactors

totient :: Int -> Int
totient n = foldl (\v (p, m) -> v * (p - 1) * p ^ (m - 1)) 1 (primeFactorsMult n)