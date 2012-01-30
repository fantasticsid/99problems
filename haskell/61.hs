data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Eq, Show)

countLeaves :: (Eq a) => Tree a -> Int
countLeaves Empty = 0
countLeaves (Branch v l r)
  | (l == Empty && r == Empty) = 1
  | otherwise = countLeaves l + countLeaves r
