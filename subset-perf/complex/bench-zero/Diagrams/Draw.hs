module Diagrams.Draw (draw) where

import Diagrams.Prelude
import Diagrams.Backend.SVG
import Data.Colour.Palette.ColorSet

type DLine = [Double]

draw :: [[Integer]] -> IO ()
draw input = do
   renderSVG "graph.svg" (dims $ V2 900 400) $ graph samples
   where
      samples = map fromIntegral <$> input

      graph :: [DLine] -> Diagram B
      graph ss = foldr1 atop $ zipWith (# lc) palette $ line <$> ss

      line :: DLine -> Diagram B
      line = lw 2 . fromVertices . map p2 . zip [0,1..] . map dscale

      dscale v = v * 5 / mx
      mx = maximum $ concat samples

palette :: [Kolor]
palette = rybColor . flip mod 24 <$> [0,5..]

