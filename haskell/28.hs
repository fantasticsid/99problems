import Data.List


lsort :: [[a]] -> [[a]]
lsort [] = []
lsort (x:[]) = [x]
lsort (x:xs) = lesser ++ [x] ++ greater
             where lesser = lsort $ filter ((< (length x)).length) xs
                   greater = lsort $ filter ((>= (length x)).length) xs

runlength :: (Eq a) => [a] -> [(Int, a)]
runlength (x:[]) = [(1, x)]
runlength (x:xs)
  | x == head xs = ((+1) $ fst $ head $ runlength xs, x):(tail $ runlength xs)
  | otherwise = (1, x):(runlength xs)

predsort :: (a -> a -> Bool) -> [a] -> [a]
predsort _ [] = []
predsort _ (x:[]) = [x]
predsort p (x:xs) = lesser ++ [x] ++ greater
  where lesser = predsort p (filter (p x) xs)
        greater = predsort p (filter (not.(p x)) xs)

lfsort :: (Eq a) => [[a]] -> [[a]]
lfsort [] = []
lfsort (x:[]) = [x]
lfsort l  = let lenlst = predsort (>) (map (\x -> length x) l)
                freq = runlength lenlst
                sortfreq = predsort (\x y -> (fst x) > (fst y)) freq
                rarelen = (snd . head) sortfreq
                fstele = head $ dropWhile (\x -> length x /= rarelen) l
            in fstele:lfsort (delete fstele l)
