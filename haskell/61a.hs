data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Eq, Show)

leaves :: (Eq a) => Tree a -> [a]
leaves Empty = []
leaves (Branch v Empty Empty) = [v]
leaves (Branch v l r) = leaves l ++ leaves r
