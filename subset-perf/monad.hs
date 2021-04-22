import Control.Monad

main :: IO ()
main = print $ length (s [0..pred n]) == 2^n
   where
   n = 27

s = filterM (const [True,False])
