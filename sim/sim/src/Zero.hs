module Zero where

-- modular index
(#) :: [a] -> Int -> a
[] # _ = error "[] # _"
t # i = t !! index
   where
      index = mod i (length t)

-- count
count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)
