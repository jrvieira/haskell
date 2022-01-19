import Zero.Queue

test :: IO ()
test = do
   putStr "dfs: "
   print $ dfs t
   putStr "bfs: "
   print $ bfs t

t = Node "root"
   (Node "0"
      (Node "00" (Leaf "000") (Leaf "001"))
      (Node "01"
         (Node "010" (Leaf "0100") (Leaf "0101"))
         (Leaf "011")
      )
   )
   (Node "1"
      (Node "10" (Leaf "100") (Leaf "101"))
      (Leaf "11")
   )

-- Binary Tree

data Tree a = Leaf a | Node a (Tree a) (Tree a)

dfs :: Tree a -> [a]
dfs = go mempty
   where
   go :: [a] -> Tree a -> [a]
   go acc (Leaf v) = v : acc
   go acc (Node v l r) = v : go (go acc r) l

bfs :: Tree a -> [a]
bfs t = go (queue [t]) t
   where
   go :: Queue (Tree a) -> Tree a -> [a]
   go q n
      | Nothing <- dequeue q = []
      | Node _ l r <- n = val x : go (r >| l >| q') x
      | Leaf _     <- n = val x : go q' x
      where
      Just (x,q') = dequeue q
      val (Leaf v) = v
      val (Node v _ _) = v

