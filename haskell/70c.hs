data Tree a = Node a [Tree a]
              deriving (Eq, Show)


tree1 = Node 'a' []

tree2 = Node 'a' [Node 'b' []]

tree3 = Node 'a' [Node 'b' [Node 'c' []]]

tree4 = Node 'b' [Node 'd' [], Node 'e' []]

tree5 = Node 'a' [
                Node 'f' [Node 'g' []],
                Node 'c' [],
                Node 'b' [Node 'd' [], Node 'e' []]
                ]

nnodes :: Tree a -> Int
nnodes (Node v []) = 1
nnodes (Node v l) = 1 + (sum $ map nnodes l)
