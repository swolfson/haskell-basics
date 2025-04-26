module Ch7 where

bindExp :: Integer -> String
bindExp x =
    let x = 10; y = 5 in
        "the integer was: " ++ show x
        ++ " and y was: " ++ show y

mTh1 x y z = x * y * z
mTh2 x y = \z -> x * y * z
mTh3 x  = \y -> \z -> x * y * z
mTh4 = \x -> \y -> \z -> x * y * z
