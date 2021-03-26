module Data.Verse where

import Data.Type

import Data.Graph

-- dimentions

(w,h) = (17,9) :: (Int,Int)

-- elements

data Element = Α | Ω | Φ | Ε | Ψ
   deriving (Show,Eq,Ord,Enum)

-- units

data Unit = Id | Focus | Light | Plant | Cat
   deriving (Eq,Ord)

-- plane

type Π = Γ Point
type Γ a = (Graph , Vertex -> Ν a , Int -> Maybe Vertex)
type Ν a = (a,Int,[Int])

data Point = Void | Hex { υ :: Set' Unit } | Tile { ξ :: Map' Element }

data Layer = Superficial | Schematic | Elemental Element

-- interact

data Universe = Universe { π :: Π , ι :: Interface }
data Interface = Interface { λ :: Layer , φ :: Some , θ :: Set' Some }

