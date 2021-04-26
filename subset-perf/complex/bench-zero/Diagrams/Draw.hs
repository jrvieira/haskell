module Draw (draw) where

import Diagrams.Prelude
import Diagrams.Backend.SVG
import Data.Colour.Palette.ColorSet

type DLine = [Double]

draw :: [DLine] -> IO ()
draw input = do
   let mx = maximum $ concat input
   renderSVG "graph.svg" (dims mx) $ graph samples
   where
      graph :: [DLine] -> Diagram B
      graph ss = foldr1 atop $ zipWith (# lc) palette $ line <$> ss

      line :: DLine -> Diagram B
      line = lw 10 . fromVertices . map p2 . zip [0,1..] . map dscale

      dscale v = negate $ v * 1 / mx

palette :: [Kolor]
palette = rybColor . flip mod 24 <$> [0,5..]

