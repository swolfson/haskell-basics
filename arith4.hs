module Arith4 where


roundTrip :: (Show a, Read b) => a -> b
roundTrip = read . show 

main = do
  print ((roundTrip :: Int -> Int) 4)
  print (id 4)
