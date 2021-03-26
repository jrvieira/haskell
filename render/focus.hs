{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Prelude hiding (take,drop)

import Control.Monad
import Data.List hiding (take,drop)
import Data.List.Split

take = genericTake
drop = genericDrop
chunks n xs = chunksOf (fromIntegral n) xs
lastN n xs = foldl' (const . drop 1) xs (drop n xs)

newtype Unit = Unit Int
   deriving (Show,Num,Integral,Bounded,Real,Enum,Ord,Eq)

-- levels as proportion of Unit on our scale
levels :: Unit
levels = 8

-- conversion must always be from Word -> Level
newtype Lvl = Lvl { val :: Unit }

instance Show Lvl where
   show l = show (lvl l) <> " (" <> show (val l) <> ")"

-- get level
lvl :: Lvl -> Unit
lvl (Lvl u) = div (u * levels) maxBound

-- rules of addition (avoiding overflows):
-- if lvl a + lvl b > levels = maxBound

-- avoid overflows
--instance Semigroup Unit where
--   a <> b = min (lvl a + lvl b) levels

--data Tile = Tile { α :: Unit , ω :: Unit , ε :: Unit , φ :: Unit , ψ :: Unit }
--
--instance Semigroup Tile where
--   a <> b = Tile $ on (<>) α a b
--
--instance Monoid Tile where
--   mempty = Tile [minBound..maxBound]

--f n = min n levels

type Coord = (Unit,Unit)
type Image a = Coord -> a

data Grid a = Grid { w :: Unit , γ :: [a] }

instance Functor Grid where
   fmap f g = g { γ = f <$> γ g }

--instance Foldable Grid where
--   foldr f z g = foldr f z (γ g)

instance Show a => Show (Grid a) where
   show (Grid w a) = join $ join $ intersperse ["\n"] $ chunks w $ (lastN 2 . (const "  " <> show)) <$> a

render :: Grid Coord -> Image a -> Grid a
render = flip fmap

main :: IO ()
main = print $ render (grid 32 32) (
   --cast (16,16) 16 1 <>
   dist (15,15)
   )

grid :: Unit -> Unit -> Grid Coord
grid w h = Grid w $ flip (,) <$> take w [0..] <*> take h [0..]

hypt :: Coord -> Unit
hypt (x,y) = round $ sqrt $ fromIntegral (x^2 + y^2)

dist :: Coord -> Image Unit
dist (xo,yo) (x,y) = hypt (xo - x,yo - y)

-- cast influence
-- source -> radius -> ease
cast :: Coord -> Unit -> Unit -> Image Unit
cast o r d t = div (x * x) r 
   where
   x = dist o t

