main :: IO ()
main = length $ s [0..31]

s [] = [[]]
s (x:xs) = map (x :) (s xs) ++ (s xs)

-- s [] = [[]]
-- s (x:xs) = map (x :) ss ++ ss
--    where
--    ss = s xs

-- s [] = [[]]
-- s (x:xs) = ss x (s xs)
--    where
--    ss _ [] = []
--    ss x (y:ys) = (x:y) : y : ss x ys
