module Verse.Plane where

import Data.Verse

import Data.Graph

   {-

   o = Tile : triangular tiles, 3rd degree vertexes of hex graph
   * = Hex : lattice points

   o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o
     *   *   *   *   *   *   *   *   *   *   *   *   *   *   *
   o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o
   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *
   o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o
     *   *   *   *   *   *   *   *   *   *   *   *   *   *   *
   o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o
   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *
   o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o
     *   *   *   *   *   *   *   *   *   *   *   *   *   *   *
   o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o

   Each Tile connects to its 3 adjacent Tiles and Hex points so
   edges between Tiles are bidirectional and
   edges between Tiles and Hex points are unidirectional.

   o <-> o 3
   o  -> * 3

   No pair of Hex points is directly connected.

   -}

-- type Vertex = Int -- in Data.Graph

plane :: Π
plane = graphFromEdges $ take (w*h) $ node <$> [0..]

node :: Int -> Ν Point
node n
   | hex = (Hex mempty,n,[])
   | dn = (Tile mempty,n,[pred n,succ n,n+2*w,n-w,pred (n+w),succ (n+w)])
   | up = (Tile mempty,n,[pred n,succ n,n-2*w,n+w,pred (n-w),succ (n-w)])
   | otherwise = (Void,n,[])
   where
   row = div n w
   _col = rem n w
   tile = even row
   shift = div row 2
   dn = tile && even shift
   up = tile && not dn
   hex = not tile && odd (shift + n)

