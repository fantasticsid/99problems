insertAt :: a -> [a] -> Int -> [a]
insertAt x xs 1 = x:xs
insertAt x xs n =  head xs : (insertAt x (tail xs) (n-1))