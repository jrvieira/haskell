import Control.Arrow

main :: IO ()
main = do
   print $ runState stat 0
   print $ runState stat (-7)

stat :: State Int Bool
stat = do
   x <- increase True -- increase and return True (1)
   y <- increase False -- increase and return False (2)
   decrease False -- decrease False (1) (False is stored but not used)
   x' <- decrease x -- decrease x (True) and return False (0)
   decrease y -- decrease y (False) (-1) (False is stored but not used)
   pure True -- change Bool (False) maintaining state (-1) (False is stored but not used)
   p <- pure $ y || x -- change Bool (y || x = True) maintaining state (-1)
   m <- not <$> increase p -- increase state and change Bool (not True = False) (0)
   decrease m -- decrease m (False) (-1)
   increase' -- increase and let Bool depend on state (> 0) (0) (False)
   increase' -- increase and let Bool depend on state (> 0) (1) (True)

-- This is the State Monad

newtype State s a = State { runState :: s -> (a,s) }

-- update state and leave a value

increase :: Bool -> State Int Bool
increase b = State $ \s -> (b,succ s)

decrease :: Bool -> State Int Bool
decrease b = State $ \s -> (b,pred s)

-- or make it state dependent

increase' :: State Int Bool
increase' = State $ \s -> let s' = succ s in (s' > 0,s')

decrease' :: State Int Bool
decrease' = State $ \s -> let s' = pred s in (s' > 0,s')

-- you can do all sorts of things

instance Functor (State s) where
-- fmap :: (a -> b) -> State s a -> State s b
   fmap f (State k) = State $ \s -> let (a,t) = k s in (f a,t)

instance Applicative (State s) where
-- pure :: a -> State s a
   pure a = State $ \s -> (a,s)
-- (<*>) :: State s (a -> b) -> State s a -> State s b
   State f <*> State a = State $ \s -> let (g,t) = f s ; (z,u) = a t in (g z,u)

instance Monad (State s) where
-- return :: a -> State s a
-- return = pure
-- (>>=) :: State s a -> (a -> State s b) -> State s b
   State f >>= k = State $ \s -> let (a,s') = f s in runState (k a) s'

