slice :: [a] -> Int -> Int -> [a]
slice (x:xs) 1 e = if length (x:xs) == e
                   then (x:xs)
                   else slice (init (x:xs)) 1 e
slice (x:xs) s e = slice xs (s-1) (e-1)