main :: IO ()
main = do
   print $ typeOf True
   print $ typeOf 'a'
   print $ typeOf (True,'a')

   print (typeOfA :: Bool)
   print (typeOfA :: Char)
   --print $ typeOfA :: (Bool,Char)

-- trivial

class TypeOf a where
   typeOf :: a -> String

instance TypeOf Bool where
   typeOf _ = "Bool"

instance TypeOf Char where
   typeOf _ = "Char"

instance (TypeOf a,TypeOf b) => TypeOf (a,b) where
   typeOf (a,b) = "(" <> typeOf a <> "," <> typeOf b <> ")"

-- ambiguous

class TypeOfA a where
   typeOfA :: String

instance TypeOfA Bool where
   typeOfA = "Bool"

instance TypeOfA Char where
   typeOfA = "Char"

