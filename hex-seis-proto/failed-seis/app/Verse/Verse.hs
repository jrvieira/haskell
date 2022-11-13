module Verse.Verse where

import Verse.Type
import Data.Graph

   {-

   o = Tile : triangular tiles, 3rd degree vertexes of hex graph
   * = Hex : hexagonal lattice points

   o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o
     *   *   *   *   *   *   *   *   *   *   *   *   *   *   *
   o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o
   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *
   o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o

   Each Tile connects to its 3 adjacent Tiles and Hex points so
   edges between Tiles are bidirectional and
   edges between Tiles and Hex points are unidirectional.

   o <-> o 3
   o  -> * 3

   No pair of Hex points is directly connected.

   -}

-- type Vertex = Int -- in Data.Graph

-- dimentions

(width,height) = (19,16) :: (Int,Int)

-- elements

data Element = Α | Ω | Φ | Ε | Ψ
   deriving (Show,Eq,Ord,Enum)

-- units

data Unit = Id | Focus | Light | Plant | Cat
   deriving (Eq,Ord)

-- verse

data Point = Void | Tile { ξ :: Map' Element } | Hex { υ :: Set' Unit }

symbol :: Point -> Char
symbol p
   | Void <- p = '.'
   | Tile _ <- p = 'o'
   | Hex _ <- p = '*'

type Verse = Grid Point
type Grid a = (Graph , Vertex -> Node a , Int -> Maybe Vertex)
type Node a = (a,Int,[Int])

data Layer = Superficial | Schematic | Elemental Element

-- interact

data Universe = Universe { π :: Verse , ι :: Interface }
data Interface = Interface { λ :: Layer , φ :: Some , θ :: Set' Some }

verse :: Verse
verse = graphFromEdges $ take (width*height) $ node <$> [0..]

node :: Int -> Node Point
node n
   | hex = (Hex mempty,n,[])
   | dn = (Tile mempty,n,[pred n,succ n,n+2*width,n-width,pred (n+width),succ (n+width)])
   | up = (Tile mempty,n,[pred n,succ n,n-2*width,n+width,pred (n-width),succ (n-width)])
   | otherwise = (Void,n,[])
   where
   row = div n width
   _col = rem n width
   tile = even row
   shift = div row 2
   dn = tile && even shift
   up = tile && not dn
   hex = not tile && odd (shift + n)
