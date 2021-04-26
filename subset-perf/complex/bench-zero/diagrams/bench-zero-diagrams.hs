import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

main :: IO ()
main = do
   samples <- read <$> pure "[[10,20,12,9.0,12,8,7,24,6,20,12,9.0,12,8,7,24,6]]"
   mainWith $ graph samples

graph ss = line $ head ss

line :: [Double] -> Diagram B
line vs = fromVertices $ p2 <$> vs'
   where
   vs' = zip [0,1..] $ (\x -> x * 10 / (maximum vs)) <$> vs 

