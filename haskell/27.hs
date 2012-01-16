import Data.List

combinations :: (Eq a) => Int -> [a] -> [[a]]
combinations n l
  | n == length l = [l]
  | n > length l = []
  | n == 0 = [[]]
  | otherwise = let withh = map (\y -> (head l):y) $ combinations (n-1) (tail l)
                    withouth = combinations n (tail l)
                in withh ++ withouth

mulgroup :: (Eq a) => [Int] -> [a] -> [[[a]]]
mulgroup (x:[]) l = map (\y ->  [y]) $ combinations x l
mulgroup (x:xs) l = concat $ map (\c -> merge c (mulgroup xs (l \\ c))) (combinations x l)
                    where merge c g = map (\x -> (c:x)) g

-- merge :: [a] -> [[[a]]] -> [[[a]]]
-- merge c g = map (\x -> (c:x)) g
