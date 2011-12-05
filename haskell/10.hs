encode :: (Eq a) => [a] -> [(Int, a)]
encode (x:[]) = [(1, x)]
encode (x:xs) = if x == head xs
                then (1 + fst (head (encode xs)), x):(tail (encode xs))
                else  (1, x):(encode xs)