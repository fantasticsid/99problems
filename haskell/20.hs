removeAt :: Int -> [a] -> [a]
removeAt 0 l = tail l
removeAt n (x:xs) = x:(removeAt (n-1) xs)