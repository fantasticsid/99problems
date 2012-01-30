data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Eq, Show)

hbalTree :: a -> Int -> [Tree a]
hbalTree v 0 = [Empty]
hbalTree v 1 = [Branch v Empty Empty]
hbalTree v n = [Branch v l r | l <- (hbalTree v (n-1)), r <- (hbalTree v (n-2))] ++
               [Branch v l r | l <- (hbalTree v (n-2)), r <- (hbalTree v (n-1))] ++
               [Branch v l r | l <- (hbalTree v (n-1)), r <- (hbalTree v (n-1))]

minNodes' :: Int -> (Int, Int)
minNodes' 0 = (0, 0)
minNodes' 1 = (0, 1)
minNodes' n = let (n'', n') = minNodes' (n-1)
              in (n', n' + n'' + 1)

minNodes :: Int -> Int -- Minimum number of nodes in a height balanced tree
minNodes n = snd $ minNodes' n

maxNodes :: Int -> Int -- Maximum number of nodes in a height balanced tree
maxNodes n = 2 ^ n - 1

numNodes :: Tree a -> Int
numNodes Empty = 0
numNodes (Branch v l r) = 1 + (numNodes l) + (numNodes r)

hbalTreeNodes :: Char -> Int -> [Tree Char]
hbalTreeNodes c n = let interval = [(h, minNodes h, maxNodes h) | h <- [1..]]
                        prune = takeWhile (\(h, min, max) -> n >= min) $ dropWhile (\(h, min, max) -> n > max) interval
                        heights = map (\(h, _, _) -> h) prune
                    in filter ((== n) . numNodes) $ concat $ map (hbalTree c) heights
