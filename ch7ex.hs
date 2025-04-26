module Ch7Ex where

tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d     = xLast `mod` 10

tensDigit1 :: Integral a => a-> a
tensDigit1 x = d
  where d1 = fst ( x `divMod` 10 )
        d  = snd ( d1 `divMod` 10 )

hundsDigit :: Integral a => a -> a
hundsDigit x = d2
  where d  = fst (x `divMod` 10 )
        d1 = fst (d `divMod` 10 ) 
        d2 = snd (d1 `divMod` 10)

foldBool1 :: a -> a -> Bool -> a
foldBool1 x1 x2 True = x1
foldBool1 x1 x2 False = x2

foldBool2 :: a -> a -> Bool -> a
foldBool2 x1 x2 test
  | test == True  = x1
  | test == False = x2

g :: (a->b) -> (a,c) -> (b,c)
g f (x,y) = (,) (f x) y

roundTrip :: (Show a, Read a) => a -> a
roundTrip x = read (show x)

roundTrip' :: (Show a, Read a) => a -> a
roundTrip' = read . show 
