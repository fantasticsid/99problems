-- data NestedList a = Multiple Int a (NestedList a) | Single a (NestedList a) | Empty deriving (Show)

-- concatList :: (Eq a) => a -> NestedList a -> NestedList a
-- concatList x (Multiple n y l) = if x == y
--                               then Multiple (n+1) y l
--                               else Single x (Multiple n y l)
-- concatList x (Single y l) = if x == y
--                           then Multiple 2 y l
--                           else Single x (Single y l)

-- encodeModified :: (Eq a) => [a] -> NestedList a
-- encodeModified [] = Empty
-- encodeModified (x:[]) = Single x Empty
-- encodeModified (x:xs) = let l = encodeModified xs
--                         in concatList x l

data EncodeItem a = Multiple Int a | Single a deriving (Show)

concatList :: (Eq a) => a -> [EncodeItem a] -> [EncodeItem a]
concatList x [] = [Single x]
concatList x ((Multiple n y):ys) = if x == y
                                  then (Multiple (n+1) y):ys
                                  else (Single x):(Multiple n y):ys
concatList x ((Single y):ys) = if x == y
                                then (Multiple 2 y):ys
                                else (Single x):(Single y):ys

encodeModified :: (Eq a) => [a] -> [EncodeItem a]
encodeModified [] = []
encodeModified (x:xs) = let l = encodeModified xs
                         in concatList x l