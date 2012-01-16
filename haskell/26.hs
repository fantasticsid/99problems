import Data.List


-- combinations :: Int -> [a] -> [[a]]
-- combinations n l
--   | n == length l = [l]
--   | n > length l = []
--   | otherwise = concat $
--                 map (\x ->
--                       ((map (\y -> x:y) (combinations (n-1) (delete x l)))
--                       ++
--                       combinations n (delete x l)))
--                 l

combinations :: (Eq a) => Int -> [a] -> [[a]]
combinations n l
  | n == length l = [l]
  | n > length l = []
  | n == 0 = [[]]
  | otherwise = let withh = map (\y -> (head l):y) $ combinations (n-1) (tail l)
                    withouth = combinations n (tail l)
                in withh ++ withouth
