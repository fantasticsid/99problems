isPrime :: Int -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n = and [n `mod` s /= 0 | s <- [2..n-1]]

myGCD :: Int -> Int -> Int
myGCD a b
  | a < 0 || b < 0 = myGCD (abs a) (abs b)
  | a > b = myGCD b a
  | otherwise = let r = mod b a
                in if r == 0
                   then a
                   else myGCD r a

coprime :: Int -> Int -> Bool
coprime a b = gcd a b == 1

primeFactors :: Int -> [Int]
primeFactors 1 = []
primeFactors n = let m = head $ filter (\x -> isPrime x && n `mod` x == 0) [2..n]
                 in m:(primeFactors $ div n m)