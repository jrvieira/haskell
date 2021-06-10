
main :: IO ()
main = do
   print $ fLazy $ Lazy 7
   print $ fStrict $ Strict 7
   print $ fNewtype $ Newtype 7

-- print $ length $ fLazy    $         undefined -- Exception
   print $ length $ fLazy    $ Lazy    undefined -- whnf ok

-- print $ length $ fStrict  $         undefined -- Exception
-- print $ length $ fStrict  $ Strict  undefined -- Exception

   print $ length $ fNewtype $         undefined -- wow *
   print $ length $ fNewtype $ Newtype undefined -- same as above, Newtype contructor is doesn't exist

   -- * is the same as:
   print $ length $ f        $         undefined

data    Lazy    = Lazy    Int
data    Strict  = Strict !Int
newtype Newtype = Newtype Int

fLazy    (Lazy    x) = [1,x]
fStrict  (Strict  x) = [1,x]
fNewtype (Newtype x) = [1,x] -- Newtype is not failing pattern match because it doesn't exist
f                 x  = [1,x]

