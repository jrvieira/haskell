import Control.Applicative (liftA2)
import Control.Monad (liftM2)

main :: IO ()
main = do
   print $ (+ 7) <$> Id 9
   print $ (+) <$> Id 7 <*> Id 9
   print $ liftA2 (+) (Id 7) (Id 9)
   print $ liftM2 (+) (Id 7) (Id 9)
   print $ Id 9 >>= pure . (+ 7)
   print $ do
      x <- Id 9
      y <- Id 7
      pure (x + y)

data Id a = Id a
   deriving Show

instance Functor Id where
   fmap f (Id x) = Id (f x)

instance Applicative Id where
   pure x = Id x
   Id f <*> Id x = Id (f x)

instance Monad Id where
   Id x >>= f = f x

