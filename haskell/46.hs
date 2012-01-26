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

table :: (Bool -> Bool -> Bool) -> [(Bool, Bool, Bool)]
table f =
  [(True, True, f True True),
   (True, False, f True False),
   (False, True, f False True),
   (False, False, f False False)]
