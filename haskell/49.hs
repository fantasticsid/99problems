gray :: Int -> [String]
gray 1 = ["0", "1"]
gray n = (map (\s -> '0':s) (gray (n-1))) ++ (map (\s -> '1':s) (reverse (gray (n-1))))