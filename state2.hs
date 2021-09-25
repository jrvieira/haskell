import Control.Monad.State.Strict

main :: IO ()
main = do
   print $ runState bols []

bols :: State [Bool] Int
bols = do
   true
   fals
   fals
   true
   fals

true :: State [Bool] Int
true = state $ \s -> let s' = True:s in (length $ filter id s',s')

fals :: State [Bool] Int
fals = state $ \s -> let s' = False:s in (length $ filter id s',s')

