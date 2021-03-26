main :: IO ()
main = do
   print $ fn (co length (F (:"yz"))) 'x'
   print $ fn (cn ((:"yx") <?> C (F length))) 'x'
   print $ fn (cn (filter (< 0) <?> C (F length))) [-1..7]
   -- predicate
   print $ p (filter (< 0) <?> P null) [-1..7]

class Covariant f where -- Functor
   co :: (a -> b) -> f a -> f b -- fmap

class Contravariant f where
   ct :: (a -> b) -> f b -> f a

infixl 4 <?>
(<?>) :: Contravariant f => (a -> b) -> f b -> f a
(<?>) = ct

--

data F a b = F { fn :: a -> b }
newtype C b a = C { cn :: F a b }

instance Covariant (F a) where
   co g (F f) = F (g . f)

instance Contravariant (C a) where
   ct g (C (F f)) = C (F (f . g))

-- Predicate

data P a = P { p :: a -> Bool }

instance Contravariant P where
   ct g (P f) = P (f . g)

