data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Eq, Show)

add :: Tree Int -> Int -> Tree Int
add Empty v = Branch v Empty Empty
add (Branch rv l r) v
  | v <= rv = Branch rv (add l v) r
  | otherwise = Branch rv l (add r v)

construct :: [Int] -> Tree Int
construct xs = foldl add Empty xs
