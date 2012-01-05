split :: [a] -> Int -> ([a], [a])
split (x:xs) n = if n == 0
                 then ([], x:xs)
                 else (x:(fst $ split xs $ n-1), snd $ split xs $ n-1)
