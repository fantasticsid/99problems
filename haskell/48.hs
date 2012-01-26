and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False

or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _ = True

nand' :: Bool -> Bool -> Bool
nand' a b = not $ and' a b

nor' :: Bool -> Bool -> Bool
nor' a b = not $ or' a b

xor' :: Bool -> Bool -> Bool
xor' True False = True
xor' False True = True
xor' _ _ = False

impl' :: Bool -> Bool -> Bool
impl' True False = False
impl' _ _ = True

equ' :: Bool -> Bool -> Bool
equ' a b
  | a == b = True
  | otherwise = False

infixl 3 `equ'`
infixl 4 `or'`
infixl 4 `nor'`
infixl 4 `xor'`
infixl 4 `impl'`
infixl 5 `and'`
infixl 5 `nand'`

boolPerm :: Int -> [[Bool]]
boolPerm 1 = [[True], [False]]
boolPerm n = concat $ map (\l -> [True:l, False:l]) (boolPerm (n-1))

tablen :: Int -> ([Bool] -> Bool) -> [[Bool]]
tablen n f = map (\l -> l ++ [f l]) (boolPerm n)