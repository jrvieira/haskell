{-# language GADTs, DataKinds, StandaloneDeriving #-}
{-# options_ghc -Wall #-}

main :: IO ()
main = do
   print "ok"

newtype State s a where
   State :: { σ :: s -> (s,a) } -> State s a

instance Functor (State s) where
   fmap f (State s) = State (fmap f . s)

instance Applicative (State s) where
   pure = State . flip (,)
   sf <*> sx = State (\s -> let (s',f) = σ sf s ; (s'',x) = σ sx s' in (s'',f x))

instance Monad (State s) where
   sx >>= f = State (\s -> let (s',x) = σ sx s in σ (f x) s')

-- --

data Tree a where
   Leaf :: a -> Tree a
   Node :: Tree a -> Tree a -> Tree a

tree :: Tree Char
tree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')
