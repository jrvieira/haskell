{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, FlexibleContexts, TypeFamilies #-}

module Fn where

main :: IO ()
main = do
   print $ meth (undefined :: Int)
   print $ meth (undefined :: Bool)
   print $ meth (undefined :: [Int])
   print $ meth (undefined :: String)
   print $ f (7 :: Int)
   print $ f 'c'
   pure ()

class C a where
   meth :: a -> String

instance C Int where
   meth _ = "int"

instance C Bool where
   meth _ = "bool"

instance C Char where
   meth _ = "char"

instance {-# INCOHERENT #-} C String where
   meth _ = "string"

instance a ~ Int => C [a] where
   meth _ = "list " ++ meth (undefined :: a)

instance {-# OVERLAPPING #-} C [Int] where
   meth _ = "list int"

f x = meth [x]

