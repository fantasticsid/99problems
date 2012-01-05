data EncodeItem a = Multiple Int a | Single a deriving (Show)

decodeHelper :: (Eq a) => EncodeItem a -> [a] -> [a]
decodeHelper (Single x) ys = x:ys
decodeHelper (Multiple n x) ys = (replicate n x) ++ ys

decodeModified :: (Eq a) => [EncodeItem a] -> [a]
decodeModified = foldr decodeHelper []