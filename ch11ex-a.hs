module Ch11exA where

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

postOrder :: BinaryTree
module Ch11exA where

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
