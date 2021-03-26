main :: IO ()
main = do
   print does
   print does'

does' :: Maybe String
does' = lookup 0 d 
   >>= \x -> lookup 1 d
   >>= \y -> lookup 2 d
   >>= \z -> pure $ unwords [x,y,z]

does :: Maybe String
does = do
   x <- lookup 0 d
   y <- lookup 1 d
   z <- lookup 2 d
   pure $ unwords [x,y,z]

d :: [(Int,String)]
-- change a key to shortcircuit monadic chains into Nothing
d = [(0,"zero"),(1,"one"),(2,"two")]

