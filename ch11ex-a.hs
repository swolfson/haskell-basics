module Ch11exa where

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b <  a = Node (insert' b left) a right
  | b >  a = Node left a (insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = 
  Node (Node Leaf 3 Leaf)
       1
       (Node Leaf 4 Leaf)

testTreeRes' :: BinaryTree Integer
testTreeRes' = 
  Node (Node Leaf 4 Leaf)
       2
       (Node Leaf 5 Leaf)

mapWorks =
  if mapTree (+1) testTree' == testTreeRes'
  then print "Yup works!"
  else print "nope!"


preOrder :: BinaryTree a -> [a]
preOrder = undefined


testTree' :: BinaryTree Integer
testTree' = 
  Node (Node Leaf 3 Leaf)
       1
       (Node Leaf 4 Leaf)

testTreeRes' :: BinaryTree Integer
testTreeRes' = 
  Node (Node Leaf 4 Leaf)
       2
       (Node Leaf 5 Leaf)

mapWorks =
  if mapTree (+1) testTree' == testTreeRes'
  then print "Yup works!"
  else print "nope!"
  

doubleUp :: [a] -> [a]
doubleUp [] = []
doubleUp xs@(x:_) = x : xs

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] [] = True
isSubseqOf s1@(c1:[]) s2@(c2:[]) = c1 == c2
isSubseqOf s1@(c1:cs1) s2@(c2:cs2) = (c1 == c2) || (isSubseqOf s1 cs2)

