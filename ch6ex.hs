module Ch6Ex where

chk :: Eq b => (a->b) -> a -> b -> Bool
chk f x y = if (f x == y) then True else False

mpr :: Integer -> Char
mpr 1 = 'a'
mpr 2 = 'b'
mpr 3 = 'c'
mpr _ = 'z'

arith :: Num b
    => (a->b)
    -> Integer
    -> a
    -> b
arith f x y = x * (f y) 

dvr = 
    
