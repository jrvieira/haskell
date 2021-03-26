module Coord where

--

w :: Int
w = 26

s :: Int
s = w*19

fps :: Int
fps = 12

--

class Pos a where
   (<+>) :: a -> a -> a
   η :: a -> a
   α :: a -> a
   β :: a -> a
   γ :: a -> a
   δ :: a -> a
   ε :: a -> a
   ζ :: a -> a

data Pnt = Pnt Int Int

data Rel = Id | Alpha | Beta | Gamma | Delta | Epsilon | Zeta

instance Pos Pnt where
   Pnt x1 y1 <+> Pnt x2 y2 = Pnt (mod (x1+x2) s) (mod (y1+y2) s)
   η p = p <+> Pnt ( 0) ( 0)
   α p = p <+> Pnt (-1) (-1)
   β p = p <+> Pnt ( 1) (-1)
   γ p = p <+> Pnt (-2) ( 0)
   δ p = p <+> Pnt ( 2) ( 0)
   ε p = p <+> Pnt (-1) ( 1)
   ζ p = p <+> Pnt ( 1) ( 1)

int :: Pnt -> Int
int (Pnt x y) = (mod x w) + mod y (div s w) * w

pnt :: Int -> Pnt
pnt i = Pnt (pntx i) (pnty i)

pntx :: Int -> Int
pntx i = mod i w

pnty :: Int -> Int
pnty i = mod (div i w) (div s w)

rel :: Rel -> Int -> Int
rel p i
   | Id      <- p = int . η $ pnt i
   | Alpha   <- p = int . α $ pnt i
   | Beta    <- p = int . β $ pnt i
   | Gamma   <- p = int . γ $ pnt i
   | Delta   <- p = int . δ $ pnt i
   | Epsilon <- p = int . ε $ pnt i
   | Zeta    <- p = int . ζ $ pnt i
