myGCD :: Int -> Int -> Int
myGCD a b
  | a < 0 || b < 0 = myGCD (abs a) (abs b)
  | a > b = myGCD b a
  | otherwise = let r = mod b a
                in if r == 0
                   then a
                   else myGCD r a
