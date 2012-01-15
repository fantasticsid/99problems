range :: Int -> Int -> [Int]
range a b
  | a > b = []
  | otherwise = a:range (a+1) b