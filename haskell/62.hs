data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Eq, Show)

internals :: Tree a -> [a]
internals Empty = []
internals (Branch v Empty Empty) = []
internals (Branch v l r) = [v] ++ internals l ++ internals r
