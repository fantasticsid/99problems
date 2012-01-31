data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Eq, Show)

layout :: Tree a -> Tree (a, (Int, Int))
layout = fst . layout' 1 1
         where layout' level order Empty = (Empty, order)
               layout' level order (Branch v l r) = let (l', order') = layout' (level+1) order l
                                                        (r', order'') = layout' (level+1) (order'+1) r
                                                    in ((Branch (v, (order', level)) l' r'), order'')
