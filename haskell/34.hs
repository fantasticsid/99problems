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

totient :: Int -> Int
totient 1 = 1
totient n = length $ filter (coprime n) [1..n-1]