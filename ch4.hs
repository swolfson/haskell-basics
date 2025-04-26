module Ch4 where

isPal :: String -> Bool
isPal str = (str == ( reverse str ))

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = (x == (reverse x))

myAbs :: Integer -> Integer
myAbs x =
    if x < 0
        then ((-1) * x)
    else
        x

f :: (a,b) -> (c,d) -> ((b,d), (a,c))
f x y = (,) nx ny
    where nx = (,) (snd x) (snd y)
          ny = (,) (fst x) (fst y)
