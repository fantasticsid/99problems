pack :: (Eq a) => [a] -> [[a]]
pack [] = [[]]
pack (x:[]) = [[x]]
pack (x:xs) = if x == head xs
              then [(x:(head (pack xs)))] ++ tail (pack xs)
              else [[x]] ++ (pack xs)