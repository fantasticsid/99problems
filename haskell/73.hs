import Data.List

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

displayLispTree :: Tree Char -> String
displayLispTree (Node v []) = [v]
displayLispTree (Node v l) = "(" ++ [v] ++ " " ++ (intercalate " " (map displayLispTree l)) ++ ")"
