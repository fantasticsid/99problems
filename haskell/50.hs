import Data.List

data HuffNode = HuffNode Int [(Char, String)] deriving Show

mergeHuff :: HuffNode -> HuffNode -> HuffNode
mergeHuff (HuffNode w1 l1) (HuffNode w2 l2) =
  HuffNode (w1 + w2) ((map (\(s, c) -> (s, '0':c)) l1) ++ (map (\(s, c) -> (s, '1':c)) l2))

huffCompare :: HuffNode -> HuffNode -> Ordering
huffCompare (HuffNode w1 l1) (HuffNode w2 l2)
  | w1 < w2 = LT
  | w1 == w2 = EQ
  | w1 > w2 = GT

huffmanEncode :: [HuffNode] -> [HuffNode]
huffmanEncode [] = []
huffmanEncode (x:[]) = [x]
huffmanEncode l = let (a:b:xs) = sortBy huffCompare l
                  in huffmanEncode ((mergeHuff a b):xs)

huffman :: [(Char, Int)] -> [(Char, String)]
huffman l = let huff = map (\(s, w) -> HuffNode w [(s, "")]) l
                huffcoding = concat $ map (\(HuffNode w l) -> l) (huffmanEncode huff)
            in huffcoding
