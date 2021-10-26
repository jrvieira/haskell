import Data.Foldable

main = do
   let ls = fromList [0..7]
   print $ ls
   print $ hed ls
   print $ til ls
   print $ foldr (:) [] ls

-- non empty list
data List a = a :~ Maybe (List a)

instance Show a => Show (List a) where
   show l = '~' : show (toList l)

fromList :: [a] -> List a
fromList (x:[]) = x :~ Nothing
fromList (x:xs) = x :~ Just (fromList xs)

hed :: List a -> a
hed (x :~ _) = x

til :: List a -> Maybe (List a)
til (_ :~ xs) = xs

instance Foldable List where
   foldr f z (x :~ Nothing) = f x z
   foldr f z (x :~ Just xs) = f x (foldr f z xs)

