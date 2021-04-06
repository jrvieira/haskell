data Tree a = Leaf a | Node a [Tree a]

leaves :: Tree a -> [a]
leaves t = go t []
   where
      go (Leaf a) = (a:)
      go (Node _ ts) = flip (foldr go) ts
