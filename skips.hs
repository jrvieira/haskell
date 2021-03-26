import Control.Arrow

main = print $ take 7 $ every 2 [0..]

--every n = uncurry (:) . (head &&& every n) . drop n
every n l = let x = drop n l in head x : every n x
--every n l = let xs@(x:_) = drop n l in x : every n xs

