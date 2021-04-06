import qualified Lib.Module as M (method)

main :: IO ()
main = print "ok"


m = M.method 1 

{-
multiline
comment
-}

-- variable declaration
x = 3 :: Int

y :: Float
y = 4

-- function declaration: explicit
function :: Int -> Int

-- function declaration: parameterized
--typevariable :: a -> a
--typeconstraint :: Num a => a -> a

-- function definition: pattern matching
function 0 = 1
function a = abs a

-- function definition: guards
function' a
    | 0 <- a = 1 -- pattern guard (mixed pattern matching + guards)
    | a < 0 = - a
    | otherwise = a

-- lists
fl :: [a] -> String
fl [] = "empty list"
fl l@(x:y:tail) = "l = list, x,y,.. = first elements, tail = rest"

-- DATA TYPES

{-
Data type declarations follow the form:
data [type constructor] = [Data (or Value) Constructor]
where value constructors are functions
-}

data Monomorphic = Nullary | Unary Int | Binary Int Int | Recursive Monomorphic
    deriving Show

data Polymorphic x = Parameterized x
    deriving Show

{-
-- same as (in generalized algebraic data type syntax):

data PolymorphicGADT x where
    ParameterizedGADT :: x -> PolymorphicGADT x

-- the advantage being that you can restrict this x like so:

data TypeConstructor parameterizedType where
    DataConstructor :: parameterizedType -> TypeConstructor parameterizedType
    Another :: Bool -> TypeConstructor Bool

-- it is strongly advised to avoid doing this
-}

data Record t tt = Record { alpha :: Int
                          , gamma :: t
                          , delta :: tt
                          }
    deriving Show

-- TYPE SYNONIMS

type Name = String
type Contact = (Name,Int)

type MakeContact = Name -> Int -> Contact

{-
This makes
    add :: String -> Int -> (String, Int)
mean the same as
    add :: Name -> Int -> Contact
and
    add :: MakeContact
-}

data Pair a b = Pair a b deriving Show
type Partial = Pair String -- Partial b == Pair Int b

-- TYPE CLASSES

class Class a where
    classical :: a -> Bool
    classical a = True

class Class a => Subclass a where
    subclass :: a -> Bool
    subclass a = True

instance Class Monomorphic where

instance (Num a) => Class (Polymorphic a)

-- value depending on types (polimorphic constant)

class Kclass a where
    kclass :: a

instance Kclass Monomorphic where
    kclass = Nullary

-- kclass :: Monomorphic == Nullary

-- monadic & do notationy

-- (>>=) :: Monad m => m a -> (a -> m b) -> m b

-- m :: Monad m => m a -> m b

-- fm = \mx -> (mx >>= \y -> pure (y + 1) >>= \z -> pure $ z + 2)
-- fm mx     =        mx >>= \y -> pure (y + 1)              >>= \z -> pure $ z + 2

{-
fm Just 6 =    Just 6 >>= \y -> pure (y + 1)              >>= \z -> pure $ z + 2
fm Just 6 =                     pure (6 + 1) :: Maybe Int >>= \z -> pure $ z + 2 -- because (>>=) :: Monad m => m a -> (a -> m b) -> m b
fm Just 6 =                     Just 7                    >>= \z -> pure $ z + 2
fm Just 6 =                                                         pure $ 7 + 2 :: Maybe Int
fm Just 6 = pure 9 :: Maybe Int
fm Just 6 = Just 9
-}

fm_do mx = do
    y <- mx
    z <- pure (y + 1)
    pure (z + 2)

{-
fm_do = do
    y <- Just 6
    z <- pure (y + 1)
    pure (z + 2)
-}

a = [1,2] >>= \n -> ['a','b'] >>= \ch -> return (n,ch)

a_do = do
    n <- [1,2]
    ch <- ['a','b']
    return (n,ch)


-- a == b == [(1,'a'),(1,'b'),(2,'a'),(2,'b')]

ma a =       [a] >>= \b -> [b]
-- ma   = \a -> [a] >>= \b -> [b]

ma_inferred a =       [a] >>= \b -> pure b -- inferred (Monadic) type
-- ma_inferred   = \a -> [a] >>= \b -> pure b -- inferred (Monadic) type
