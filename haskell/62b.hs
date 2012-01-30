data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Eq, Show)

atLevel :: Tree a -> Int -> [a]
atLevel Empty _ = []
atLevel (Branch v l r) 1 = [v]
atLevel (Branch v l r) n = (atLevel l (n-1)) ++ (atLevel r (n-1))

tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)