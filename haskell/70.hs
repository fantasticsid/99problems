import Data.Char
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

stringToTree :: String -> Tree Char
stringToTree "" = error "wrong string"
stringToTree (x:xs) = Node x $ stringToNodes $ init xs

stringToNodes :: String -> [Tree Char]
stringToNodes "" = []
stringToNodes s = map stringToTree $ splitChildren s

splitChildren :: String -> [String]
splitChildren "" = []
splitChildren s = let (h, t) = consume s
                  in h:(splitChildren t)
  where consume s = let ctr = scanl (\x c -> if isAlpha c
                                             then x+1
                                             else x-1)
                              0
                              s
                        idx = elemIndices 0 ctr
                        splitIdx = idx !! 1
                    in splitAt splitIdx s

treeToString :: Tree Char -> String
treeToString (Node v []) = v:"^"
treeToString (Node v l) = [v] ++ (concatMap treeToString l) ++ "^"
