main :: IO ()
main = do
   print $ "ok"
   let x = 1 :/ 2 :/ 3 :/ Nil :: List Int
   let y = 7 :/ 8 :/ 9 :/ Nil :: List Int
   print $ intersperse 6 x
   print $ join x y

data Will a = None | Some a

infixr :/
data List a = Nil | a :/ (List a) deriving Show

intersperse :: a -> List a -> List a
intersperse _ Nil = Nil
intersperse el (a :/ as) = a :/ el :/ intersperse el as

join :: List a -> List a -> List a
join Nil y = y
join (a :/ as) y = a :/ join as y
