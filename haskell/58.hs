data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Eq, Show)

cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree 1 = [Branch 'x' Empty Empty]
cbalTree n
  | even n = let m = div (n-1) 2
             in concatTree (cbalTree m) (cbalTree $ m+1) ++ concatTree (cbalTree $ m+1) (cbalTree m)
  | odd n = let m = div (n-1) 2
            in concatTree (cbalTree m) (cbalTree m)


concatTree :: [Tree Char] -> [Tree Char] -> [Tree Char]
concatTree lt rt = [Branch 'x' l r | l <- lt, r <- rt]

mirror :: Tree a -> Tree a -> Bool
mirror Empty Empty = True
mirror (Branch v1 l1 r1) (Branch v2 l2 r2) = and [mirror l1 r2, mirror r1 l2]
mirror _ _ = False

symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch v l r) = mirror l r

symCbalTrees :: Int -> [Tree Char]
symCbalTrees n = filter symmetric (cbalTree n)
