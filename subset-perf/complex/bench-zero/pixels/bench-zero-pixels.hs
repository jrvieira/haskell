import Codec.Picture
import Codec.Picture.Drawing
import Data.Word

testdata = "[ [20,22,15,17,28,31], [9,10,12,17,28,36], [14,14,13,12,17,15] ]"

main :: IO ()
main = do 
   input <- read <$> pure testdata
   print $ scale input

scale :: [[Int]] -> [[(Int,Int)]]
scale xs = axis $ scale <$> xs
   where
   scale = map (\x -> div (100 * x) mx)
   mx = maximum $ concat xs
   axis = map $ zip [0,10..]

type Color = Word8 -> PixelRGBA8
red :: Color
red = PixelRGBA8 255 0 0
green  :: Color
green = PixelRGBA8 0 255 0
blue  :: Color
blue = PixelRGBA8 0 0 255
yellow :: Color
yellow = PixelRGBA8 255 255 0
magenta :: Color
magenta = PixelRGBA8 255 0 255
cyan  :: Color
cyan = PixelRGBA8 0 255 255

Graph :: Graph Color [Int]

palette :: [Pixel]

