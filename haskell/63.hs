data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Eq, Show)

completeBinaryTree' :: Int -> Int -> Tree Char
completeBinaryTree' nodenum total
  | nodenum <= total = (Branch 'x' (completeBinaryTree' (nodenum * 2) total) (completeBinaryTree' (nodenum * 2 + 1) total))
  | otherwise = Empty

completeBinaryTree = completeBinaryTree' 1