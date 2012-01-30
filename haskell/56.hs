data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Eq, Show)

mirror :: Tree a -> Tree a -> Bool
mirror Empty Empty = True
mirror (Branch v1 l1 r1) (Branch v2 l2 r2) = and [mirror l1 r2, mirror r1 l2]
mirror _ _ = False

symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch v l r) = mirror l r
