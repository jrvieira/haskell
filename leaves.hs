data Tree a = Leaf a | Node a [Tree a]

leaves :: Tree a -> [a]
leaves t = go t []
   where
      go (Leaf a) = (a:)
      go (Node _ ts) = flip (foldr go) ts

-- Binary Tree

data Tree a = Leaf a | Node a (Tree a) (Tree a)

leaves :: Tree a -> [a]
-- naive
leaves (Leaf v) = [v]
leaves (Node _ l r) = leaves l ++ leaves r
-- optimized (avoids left associating concat via accumulator)
leaves t = go t []
   where
   go :: Tree a -> [a] -> [a]
   go (Leaf v) = (v:)
   go (Node _ l r) = go l . go r

