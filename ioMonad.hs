-- IO Monad

{- GHC.Base

instance Monad IO  where
    (>>)      = (*>)
    (>>=)     = bindIO
    fail s    = failIO s

bindIO :: IO a -> (a -> IO b) -> IO b
bindIO (IO m) k = IO (\ s -> case m s of (# new_s, a #) -> unIO (k a) new_s)

(*>) :: f a -> f b -> f b

-- imported from GHC.IO
failIO :: String -> IO a
failIO s = IO (raiseIO# (toException (userError s)))

-- meaning:
(>>=) :: IO a -> (a -> IO b) -> IO b
(>>) :: IO a -> IO b -> IO b 
pure :: a -> IO a
fail :: String -> IO a

-}

{-# LANGUAGE BangPatterns #-}
import System.IO.Unsafe

-- current state of the world
data World = World

-- write
-- takes a string and the state of the world and returns the new state of the world
oput :: World -> String -> World
oput !w s = unsafePerformIO (putStrLn s >> pure w)
-- read
-- takes the state of the world and returns a string and the new state of the world
iput :: World -> (World, String)
iput !w = unsafePerformIO (getLine >>= (\s -> pure (w, s)))

-- write a string, read a string and then use that input to write another string
-- notice how the state of the world is passed between oput and iput
ioPure :: World -> World
ioPure w1 = w4
    where w2      = oput w1 "output"
          (w3, i) = iput w2
          w4      = oput w3 ("output: " ++ i)

-- how about branching the state of the world?
-- this whould be impossible
-- it would mean that for the same input, we are getting different outcomes
-- theoretically, it breaks purity
split :: World -> (World, World)
split w = (oput w "o1", oput w "o2")

-- enter uniqueness typing
-- the compiler should not allow situations like this: uniqueness error

-- Haskell hides World in a transformer
type WorldT a = World -> (World, a)

-- iput is already a WorldT String
iputT :: WorldT String
iputT = iput

-- oput implementation:
oputT :: String -> WorldT ()
oputT s w = (oput w s, ())




-- we need to implement a way to compose iputT and oputT
(>>>=) :: WorldT a           -- World -> (World, a)
       -> (a -> WorldT b)    -- (a -> World -> (World, b))
       -> WorldT b           -- World -> (World, b)
(>>>=) wt f = (uncurry . flip) f . wt

-- flip :: (a -> b -> c) -> b -> a -> c
-- uncurry :: (a -> b -> c) -> (a, b) -> c

ioPureT :: WorldT ()
ioPureT =
    oputT "outputT" >>>= \_ ->
    iputT           >>>= \i ->
    oputT ("outputT:" ++ i)

-- USAGE: ioPureT World
--        > ((), "output")
--        >                     -- waiting for i
--        > ("outputT" ++ i)

instance Monad WorldT a where
  (>>=) = (>>>=)

