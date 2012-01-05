dupli :: [a] -> [a]
dupli = foldr ((++) . (\x -> [x, x])) []