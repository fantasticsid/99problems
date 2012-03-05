queens :: Int -> [[Int]]
queens n = queensGen n n

queensGen n 0 = [[]]
queensGen n m = let l = queensGen n (m-1)
                in  concatMap (appendViableEle [0..n-1]) l
  where appendViableEle poss lst = let viabPos = filter (viablePos lst) poss
                                   in map (\x -> lst ++ [x]) viabPos
        viablePos l p = let diag = map (\(a,b) -> a+b) (zip l [0..])
                            diag' = map (\(a,b) -> a-b) (zip l [0..])
                            idx = length l
                        in (not (elem p l)) && (not (elem (p+idx) diag)) && (not (elem (p-idx) diag'))
