data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Eq, Show)

hbalTree :: a -> Int -> [Tree a]
hbalTree v 0 = [Empty]
hbalTree v 1 = [Branch v Empty Empty]
hbalTree v n = [Branch v l r | l <- (hbalTree v (n-1)), r <- (hbalTree v (n-2))] ++
               [Branch v l r | l <- (hbalTree v (n-2)), r <- (hbalTree v (n-1))] ++
               [Branch v l r | l <- (hbalTree v (n-1)), r <- (hbalTree v (n-1))]
