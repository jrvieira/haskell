import Data.List

main :: IO ()
main = interact $ t12to24

t12to24 :: String -> String
t12to24 s = ((zeroPad . show) h24) ++ ":" ++ m24 ++ ":" ++ s24 where
   am = s !! 8 == 'A'
   h12 = read $ take 2 s
   m12 = (s !! 3) : (s !! 4) : []
   s12 = (s !! 6) : (s !! 7) : []
   h24 = let x = if am then 0 else 12 in (mod h12 12) + x
   m24 = m12
   s24 = s12


zeroPad :: String -> String
zeroPad i = drop (length o - 2) o where
   o = "0" ++ i
