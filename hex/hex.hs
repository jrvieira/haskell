module Hex where

import Zero.Zero
import Data.Type
import Data.Verse
import Verse.Plane (plane)
import Pixel.Render
import Pixel.Interface (step)

universe :: Universe
universe = Universe plane (Interface Superficial (Some 27) mempty)

main :: IO Universe
main = do
   echo "hex" $ "interact"
   step render universe

