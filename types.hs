module Main (main) where

main = do
  print "ok"
  print $ f' Data
  print $ f Data
  print $ fd' Data
  print $ fd Data
  print $ fg' Data
  print $ fg Data
  print $ universal' Data
  print $ universal Data
  print $ (reflexion' :: Type)
  print $ (reflexion :: Type)

-- simple class structure

data T = D

data Class = Alpha | Beta deriving (Eq, Show)

class A a where
    super :: a -> Class
    super _ = Alpha

class A a => B a where
    sub :: a -> Class
    sub _ = Beta

instance B T where
instance A T where

-- testing classes

data Type = Data | Data' deriving Show

class Super a where -- class definitions must have type variables!
   f' :: a -> String
   fd' :: a -> String
   fd' a = "Super a class default" -- in order to use show here, class would need the (Show a) constraint
   fg' :: a -> String
   fg' a = "Super a class ghost" -- in order to use show here, class would need the (Show a) constraint
   universal' :: a -> String
   universal' _ = "Super universal"
   reflexion' :: a -- (bound)

class Super a => Sub a where -- class definitions must have type variables!
   f :: a -> String
   fd :: a -> String
   fd a = "Sub a class default" -- in order to use show here, class would need the (Show a) constraint
   fg :: a -> String
   fg a = "Sub a class ghost" -- in order to use show here, class would need the (Show a) constraint
   universal :: a -> String
   universal _ = "Sub universal"
   reflexion :: a -- (bound)

instance Super Type where
   -- this instance must have an explicit implementation of all functions declared in the class, unless they are already defined there
   f' a = "Super Type:" ++ show a ++ " instance"
   -- if they are already defined, they can be ghosted:
   fg' Data = "Super Type instance (ghosted)"

   reflexion' = Data'

-- any Type must be an instance of Super before it can be an instance of Sub, so
instance Sub Type where
   -- this instance must have an explicit implementation of f
   f a = "Sub Type:" ++ show a ++ " instance"
   fg Data = "Sub Type instance (ghosted)"
   -- no match for fg Data', this is non-exautive pattern

   reflexion = Data

{-
-- if we apply class functions to Data, Haskell searches the instance declaration for definitions

 > f' Data == "Super Data instance"
 > f Data == "Sub Data instance"

-- if they are not defined there, they must be defined in the class

 > fd' Data == "Super Data class default"
 > fd Data == "Sub Data class default"

-- if defined on both instance and class, class definition is ghosted by instance

 > fg' Data == "Super Data instance (ghosted)"
 > fg Data == "Sub Data instance (ghosted)"

-- if functions are non exaustive, you get an error ()

 > fg' Data' == error "non exaustive pattern…"
 > fg Data' == error "non exaustive pattern…"

-}


data MRecord = FstR {
    one :: Int,
    two :: Int
  } | SndR {
    alpha :: String,
    beta :: String
  }

