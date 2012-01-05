dropHelper :: [a] -> Int -> Int -> [a]
dropHelper [] k n = []
dropHelper (x:xs) 1 n = dropHelper xs n n
dropHelper (x:xs) k n = x:(dropHelper xs (k-1) n)

dropEvery :: [a] -> Int -> [a]
dropEvery xs n = dropHelper xs n n
