import Pixel
import Control.Monad
import Data.List
import Data.List.Split

type Unit = Int
type Coord = (Unit,Unit)
type Image a = Coord -> a

data Grid a = Grid { w :: Int , γ :: [a] }

instance Functor Grid where
   fmap f g = g { γ = f <$> γ g }

--instance Foldable Grid where
--   foldr f z g = foldr f z (γ g)

instance Show a => Show (Grid a) where
   show (Grid w a) = join $ join $ intersperse ["\n"] $ chunksOf w $ show <$> a

newtype Dig = Dig { ι :: Int }

instance Show Dig where
   show i = show (ι i) <> " "

newtype NMap = NMap { κ :: Int }

instance Show NMap where
   show (NMap k) = clr8 BG (232 + k) (clr8 FG 0 (lastN 2 $ " " <> show (mod k 100)))

instance Semigroup NMap where
   NMap a <> NMap b = NMap (a+b)

instance Monoid NMap where
   mempty = NMap 0

lastN :: Int -> [a] -> [a]
lastN n xs = foldl' (const . drop 1) xs (drop n xs)

main :: IO ()
main = do
   print $ render (grid 9 9) (Dig . fst)
   print $ render (grid 9 9) (Dig . dist (2,3))
   print $ render (grid 9 9) (Color8 . (+232) . dist (3,3))
   print $ render (grid 9 9) (draw (circle (3,4) 3) (Pixel C) <> draw (circle (5,4) 3) (Pixel C))
   print $ render (grid 9 9) (draw (ball (4,4) 3) (Pixel C) <> draw (circle (4,4) 3) (Pixel Y))
   print $ render (grid 9 9) (draw (ball (4,4) 3) C <> draw (circle (4,4) 3) Y)
   print $ render (grid 9 9) (draw (rectangle (1,2) 6 4) W <> draw (ball (4,4) 2) R)
   print $ render (grid 9 9) (draw (rectangle (1,2) 6 4) (Pixel W) <> draw (ball (4,4) 2) (Pixel R))
   print $ render (grid 9 9) (NMap . dist (0,0) <> draw (rectangle (2,2) 7 2) (NMap 3))
   print $ render (grid 9 9) (NMap . dist (0,0) <> NMap . dist (9,9))

grid :: Int -> Int -> Grid Coord
grid w h = Grid w $ flip (,) <$> take w [0..] <*> take h [0..]

-- linear interpolation
li :: (RealFrac a,Enum a) => a -> a -> Int -> [Int]
li a b n = (round . lerp) <$> [0,1/n'..1]
   where
   lerp t = (b - a) * t + a
   n' = fromIntegral (n-1)

render :: Grid Coord -> Image a -> Grid a
render = flip fmap

hyptn :: Coord -> Int
hyptn (x,y) = round $ sqrt $ fromIntegral (x^2 + y^2)

dist :: Coord -> Image Int
dist (xo,yo) (x,y) = hyptn (xo - x,yo - y)

circle :: Coord -> Int -> Image Bool
circle (xo,yo) r (x,y) = hyptn (x - xo,y - yo) == r

ball :: Coord -> Int -> Image Bool
ball (xo,yo) r (x,y) = hyptn (x - xo,y - yo) < r

rectangle :: Coord -> Int -> Int -> Image Bool
rectangle (xo,yo) w h (x,y) = dx >= 0 && dx < w && dy >= 0 && dy < h
   where
   dx = x - xo
   dy = y - yo
   -- \(x,y) -> elem x [xo..xo+w] && elem y [yo..yo+h]

draw :: Monoid a => Image Bool -> a -> Image a
draw f x = g . f
   where
   g b
      | b = x
      | otherwise = mempty

