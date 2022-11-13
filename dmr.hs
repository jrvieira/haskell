{-# Language NoMonomorphismRestriction #-}
{-# Language PartialTypeSignatures #-}

main :: IO ()
main = print (f,h)

f :: (Int,Float)
f = (fst y, snd y)
   where
   x :: Num a => (a,a)
   x = (0,1)
-- works only no MR
   y = x

-- g :: (Int,Float)
-- g = (fst y, snd y)
--    where
--    x :: Num a => (a,a)
--    x = (0,1)
-- -- errors
--    y :: _
--    y = x

h :: (Int,Float)
h = (fst y, snd y)
   where
   x :: Num a => (a,a)
   x = (0,1)
-- works even with MR
   y :: _ => _
   y = x
