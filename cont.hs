{-# LANGUAGE RankNTypes #-}

import Control.Applicative ( (<|>) )

-- The Continuation Monad
-- https://blog.poisson.chat/posts/2019-10-27-continuation-submonads.html

main :: IO ()
main = do
   print "ok"


newtype Cont r a = Cont { cont :: (a -> r) -> r }

-- Use the identity continuation to extract the final result.
evalCont :: Cont a a -> a
evalCont (Cont m) = m id

instance Functor (Cont r) where

-- fmap :: (a -> b) -> Cont r a -> Cont r b
   fmap f (Cont m) = Cont (\ k -> m (k . f))

instance Applicative (Cont r) where

-- pure :: a -> Cont r a
   pure a = Cont (\ k -> k a)

-- (<*>) :: Cont r (a -> b) -> Cont r a -> Cont r b
   Cont mf <*> Cont ma = Cont (\k -> mf (\f -> ma (\a -> k (f a))))

instance Monad (Cont r) where

-- (>>=) :: Cont r a -> (a -> Cont r b) -> Cont r b
   Cont ma >>= mc_ = Cont (\k -> ma (\a -> mc a (\b -> k b))) where

      mc = cont . mc_

-- Continuation transformers
-- (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
-- (>=>) :: (a -> Cont r b) -> (b -> Cont r c) -> (a -> Cont r c)

-- Identity

type Done a = forall r. Cont r a
           -- forall r. (a -> r) -> r

done :: Done a -> a
done (Cont m) = m id

-- Maybe

type Abortable a = forall r. Cont (Maybe r) a
                -- forall r. (a -> Maybe r) -> Maybe r

abort :: Abortable x
abort = Cont (\_ -> Nothing)

runAbortable :: Abortable a -> Maybe a
runAbortable (Cont m) = m Just

secondGuess :: Abortable Bool
secondGuess = Cont (\k -> k True <|> k False)

pureTrue :: Abortable Bool
pureTrue = pure True

-- Either

type Except e a = forall r. Cont (Either e r) a
               -- forall r. (a -> Either e r) -> Either e r

throw :: e -> Except e a
throw e = Cont (\_ -> Left e)

runExcept :: Except e a -> Either e a
runExcept (Cont m) = m Right

-- State

type State s a = forall r. Cont (s -> r) a
              -- forall r. (a -> s -> r) -> s -> r

get :: State s s
get = Cont (\k s -> k s s)

put :: s -> State s ()
put s = Cont (\k _ -> k () s)

runState :: State s a -> s -> (a, s)
runState (Cont m) = m (,)

-- Writer

type Writer w a = forall r. Cont (w,r) a
               -- forall r. (a -> (w,r)) -> (w, r)

tell :: Monoid w => w -> Writer w ()
tell w = Cont (\k -> let (w0,r) = k () in (w <> w0,r))

runWriter :: Monoid w => Writer w a -> (w,a)
runWriter (Cont m) = m (\a -> (mempty,a))

-- State, reversed

type RState s a = forall r. Cont (s,r) a
               -- forall r. (a -> (s,r)) -> (s,r)

rmodify :: (s -> s) -> RState s ()
rmodify f = Cont (\k -> let (s,r) = k () in (f s,r))

rget :: RState s s
rget = Cont (\k -> let (s,r) = k s in (s,r))

runRState :: RState s a -> s -> (s,a)
runRState (Cont m) s = m (\a -> (s,a))

-- Tardis

type Tardis bw fw a = forall r. Cont (fw -> (bw,r)) a
                   -- forall r. (a -> fw -> (bw,r)) -> fw -> (bw,r)

-- List

type List a = forall r. Cont [r] a
           -- forall r. (a -> [r]) -> [r]

decide :: List Bool
decide = Cont (\k -> k True ++ k False)

vanish :: forall a. List a
vanish = Cont (\_ -> [])

runList :: List a -> [a]
runList (Cont m) = m (\a -> [a])

{- There’s a handful of variations for that one

-- Use NonEmpty r to rule out vanish
-- generalize over an abstract monoid or semigroup r to prevent inspection of the continuation
-- or use a Tree r to keep track of the order of choices

-- type List1  a = forall r.                Cont (NonEmpty r) a
-- type List'  a = forall r. Monoid    r => Cont r a
-- type List1' a = forall r. Semigroup r => Cont r a
-- type Tree0  a = forall r.                Cont (Tree r) a

-}

-- ContT

type ContT r m a = Cont (m r) a
                -- (a -> m r) -> m r

lift :: Monad m => m a -> ContT r m a
     -- Monad m => m a -> (a -> m r) -> m r
lift u = Cont (\k -> u >>= k)

-- Monad morphism laws:
--   lift (pure a)   =   pure a
--   lift (u >>= \a -> k a) = lift u >>= \a -> lift (k a)

-- CodensityT

type CodensityT m a = forall r. Cont (m r) a
                   -- forall r. (a -> m r) -> m r

{- "retracts" here means that there is a surjective but not injective mapping
   from the left to right

-- Done       =  CodensityT Identity    isomorphic to  Identity
-- Abortable  =  CodensityT Maybe         retracts to  Maybe
-- Except e   =  CodensityT (Either e)    retracts to  Either e
-- State s    =  CodensityT (Reader s)  isomorphic to  State s
-- Writer w   =  CodensityT (Writer w)    retracts to  Writer w, or (reverse) State w
-- Tardis s   =  CodensityT (State s)     retracts to  Tardis s
-- List       =  CodensityT []            retracts to  []

-}
