{-# LANGUAGE DeriveDataTypeable #-}

import Data.Data

{- GENERIC PROGRAMMING
with the Typeable and Data classes
-}

main :: IO ()
main = do
   print $ typeOf $ X
   print $ typeOf $ (N :: T ()) -- can't deduce a without type annotation
   print $ typeOf $ S ()
   print $ typeOf $ R () N

   print $ dataTypeOf $ X
   print $ dataTypeOf $ S ()

   print $ dataTypeConstrs $ dataTypeOf $ X
   print $ dataTypeConstrs $ dataTypeOf $ S ()

   print $ isAlgType $ dataTypeOf $ X
   print $ isAlgType $ dataTypeOf $ S ()
   print $ isAlgType $ dataTypeOf $ ' '

   print $ toConstr $ X
   print $ toConstr $ S ()
   print $ toConstr $ R () N

   print $ constrType $ toConstr $ X
   print $ constrType $ toConstr $ S ()

   print $ constrFields $ toConstr $ X
   print $ constrFields $ toConstr $ (N :: T ())
   print $ constrFields $ toConstr $ S ()
   print $ constrFields $ toConstr $ R () N
   print $ constrFields $ toConstr $ (Nil :: R ())
   print $ constrFields $ toConstr $ Single ()
   print $ constrFields $ toConstr $ Recursive () Nil

data X = X
   deriving (Typeable,Data)

data T a = N | S a | R a (T a)
   deriving (Typeable,Data)

data R a = Nil | Single { single :: a } | Recursive { recursive :: a , recursion :: R a }
   deriving (Typeable,Data)

