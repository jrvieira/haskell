module Pixel.Render where

import Data.Type
import Data.Verse

import Util
import Data.List

view :: Layer -> Point -> String
view Superficial p = view Schematic p
view Schematic p
   | Void <- p = " "
   | Hex s <- p = if has Cat s then "C" else "*"
   | Tile m <- p = "o"
view (Elemental e) p
   | Void <- p = " "
   | Hex s <- p = " "
   | Tile m <- p = show $ get e m

render :: Universe -> IO ()
render u = putStr $ (' ' :) $ intersperse ' ' $ unlines $ chunkList w $ concat $ take (w*h) $ point <$> [0..]
   where
   (_,ν,η) = π u
   point i 
      | φ (ι u) == Some i = "Φ"
      | Just (x,_,_) <- ν <$> η i = view (λ $ ι u) x
      | otherwise = error "extraplanar coordinate"

