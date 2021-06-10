--import Control.Monad.Cont
import Data.Functor.Sum

{- The Continuation Monad
-}

main :: IO ()
main = do
   print (f :: Sum Int)


f = do
   a <- pure 7
   b <- pure 9
   pure $ a + b

