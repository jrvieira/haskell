module Coord' where

--

w :: Int
w = 190

s :: Int
s = w*190

fps :: Int
fps = 12

--

data Rel = Id | Alpha | Beta | Gamma | Delta | Epsilon | Zeta

type Position = (Int,Int)

sop :: Position -> Int
sop (x,y) = (mod x w) + mod y (div s w) * w

pos :: Int -> Position
pos i = (posx i,posy i)

posx :: Int -> Int
posx i = mod i w

posy :: Int -> Int
posy i = mod (div i w) (div s w)

up :: Int -> Int
up i = sop (posx i , posy i - 1)

dn :: Int -> Int
dn i = sop (posx i , posy i + 1)

lf :: Int -> Int
lf i = sop (posx i - 1 , posy i)

rt :: Int -> Int
rt i = sop (posx i + 1 , posy i)

rel :: Rel -> Int -> Int
rel p i
   | Id <- p = mod i s
   | Alpha <- p = up . lf $ i
   | Beta <- p = up . rt $ i
   | Gamma <- p = lf . lf $ i
   | Delta <- p = rt . rt $ i
   | Epsilon <- p = dn . lf $ i
   | Zeta <- p = dn . rt $ i
