rotate :: [a] -> Int -> [a]
rotate l 0 = l
rotate (x:xs) k = if k > 0
                  then rotate (xs ++ [x]) (k-1)
                  else rotate ((last (x:xs)) : (init (x:xs))) (k+1)