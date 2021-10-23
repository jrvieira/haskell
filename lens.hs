{-# LANGUAGE RankNTypes #-}

main :: IO ()
main = do
   testL
   testC
   testF
   test

{- LENSES FROM SCRATCH
   https://www.youtube.com/watch?v=3kduOmZ2Wxw
-}

data Foo = Foo (Int,Int) Char
   deriving ( Show )

myTuple :: (Int,Int)
myTuple = (7,7)

myFoo :: Foo
myFoo = Foo myTuple 'a'

-- getter setter

getTuple :: Foo -> (Int,Int)
getTuple (Foo a _) = a

setTuple :: Foo -> (Int,Int) -> Foo
setTuple (Foo a c) b = Foo b c

getSnd :: (a,b) -> b
getSnd = snd

setSnd :: (a,b) -> b -> (a,b)
setSnd (a,_) b = (a,b)

-- naive lens

data L s a = L { getL :: s -> a , setL :: s -> a -> s }

tupleL :: L Foo (Int,Int)
tupleL = L getTuple setTuple

sndL :: L (a,b) b
sndL = L getSnd setSnd

testL :: IO ()
testL = do
   print $ getL sndL myTuple
   print $ setL sndL myTuple 7
   print $ setL tupleL myFoo (9,9)
   -- but we can't go deep

-- composable lens

type LensC s a = (a -> a) -> s -> s
-- give an endomorphism and a structure
-- get an updated structure

lensC :: (s -> a) -> (s -> a -> s) -> LensC s a
--       ^get        ^set            ^(a -> a) -> s -> s
lensC sa sas aa s = sas s $ aa $ sa s

_tupleC :: LensC Foo (Int,Int)
_tupleC = lensC getTuple setTuple

_sndC :: LensC (a,b) b
_sndC = lensC snd $ \(a,_) b -> (a,b)

-- get :: LensC s a -> s -> a
-- get l s = l id s

setC :: LensC s a -> s -> a -> s
setC l s a = l (const a) s

testC :: IO ()
testC = do
   print $ setC (_tupleC . _sndC) myFoo 9
   -- now we can compose lenses to go deeper

{- NOW
   we cant implement get because we can't extract the a
   so the solution is (with RankNTypes) using Functors
-}

type LensF s a = forall f. Functor f => (a -> f a) -> s -> f s

newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
   fmap f (Identity a) = Identity (f a)

newtype Const b a = Const { runConst :: b }

instance Functor (Const b) where
   fmap _ (Const b) = Const b

lensF :: (s -> a) -> (s -> a -> s) -> LensF s a
lensF sa sas afa s = sas s <$> afa (sa s)

_tupleF :: LensF Foo (Int,Int)
_tupleF = lensF getTuple setTuple

_sndF :: LensF (a,b) b
_sndF = lensF snd $ \(a,_) b -> (a,b)

getF :: LensF s a -> s -> a
getF l s = runConst $ l Const s

setF :: LensF s a -> s -> a -> s
setF l s a = runIdentity $ l (const $ Identity a) s

-- new function that updates an a and returns its context
updF :: LensF s a -> s -> (a -> a) -> s
updF l s f = runIdentity $ l (Identity . f) s

testF :: IO ()
testF = do
   print $ getF (_tupleF . _sndF) myFoo
   print $ setF (_tupleF . _sndF) myFoo 9
   print $ updF (_tupleF . _sndF) myFoo (+2)
   -- however, we can't change types:
-- print $ setF _sndF myTuple 'c'
-- print $ updF _sndF myTuple show

-- the solution is to further generalize Lens
-- parameterizing it by aditional output types

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

-- ((Int,Int) -> f (Int,Int)) -> Foo -> f Foo

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens sa sbt afa s = sbt s <$> afa (sa s)

_tuple :: Lens Foo Foo (Int,Int) (Int,Int)
_tuple = lens (\(Foo a _) -> a) (\(Foo a c) b -> Foo b c)

_snd :: Lens (a,b) (a,c) b c
_snd = lens snd $ \(a,_) c -> (a,c)

get :: Lens s t a b -> s -> a
get l s = runConst $ l Const s

set :: Lens s t a b -> s -> b -> t
set l s a = runIdentity $ l (const $ Identity a) s

-- now set is not necessarily endomorphic
upd :: Lens s t a b -> s -> (a -> b) -> t
upd l s f = runIdentity $ l (Identity . f) s

test :: IO ()
test = do
   print $ get (_tuple . _snd) myFoo
   print $ set (_tuple . _snd) myFoo 9
   print $ upd (_tuple . _snd) myFoo (+2)
   -- now this works
   print $ set _snd myTuple 'c'
   print $ upd _snd myTuple show

