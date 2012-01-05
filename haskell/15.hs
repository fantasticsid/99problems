repli :: [a] -> Int -> [a]
repli xs n = foldr ((++).(replicate n)) [] xs