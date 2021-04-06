{-

- The chance of guessing a binary outcome with events I | O with probability

Pe(I) = 1/η
Pe(O) = 1 - 1/η = (η-1)/η

- by guessing

Pg(I) = 1/γ
Pg(O) = 1 - 1/γ = (γ-1)/γ

- should be

e g
I I = Pe(I) * Pg(I)
O I = Pe(O) * Pg(I)
I O = Pe(I) * Pg(O)
O O = Pe(O) * Pg(O)

Pe(O) = 1 - Pe(I)
Pg(O) = 1 - Pg(O)

P(e==g) = Pe(I) * Pg(I) + Pe(O) * Pg(O)
        = Pe(I) * Pg(I) + (1 - Pe(I)) * (1 - Pg(I))
        = 1 + 2(Pe(I)*Pg(I)) - Pe(I) - Pg(I)

        = let x = Pe(I) , y = Pg(I) in 1 + 2xy - x - y

so, if η == γ :

let π = η = γ in

π(e==g) = 1 + 2/(π^2) - 2/π

as corroborated by the following Monte Carlo simulation.

-}

import Control.Monad
import System.Random
import System.Directory
import Codec.Picture

main :: IO ()
main = do
-- results <- sequence $ zipWith guess [1..d] [1..d]
-- mapM_ print $ zip expected (map (1 -) results) -- Pg(I) = 1 - Pe(I) = Pe(O)
-- mapM_ print $ zip expected' results -- η = γ
   draw

draw :: IO ()
draw = do
   createDirectoryIfMissing True "io"
   savePngImage "io/guess.png" $ ImageY8 (generateImage probability 12 12)
      where
         probability :: Int -> Int -> Pixel8
         probability e g = round $ 255 * (1 + 2*η*γ - η - γ)
            where
               η = 1 / fromIntegral (e+1) -- +1 avoids 0 coordinates
               γ = 1 / fromIntegral (g+1)

-- SIMULATION

s = 10000000 -- number of samples
d = 12 -- depth

expected :: [Double] -- 1/γ = 1 - 1/η [γ and η are conjugate indices](http://mathonline.wikidot.com/conjugate-indices-1-p-1-q-1)
expected = map (\π -> let η = 1/π ; γ = 1 - 1/π in 1 + 2*η*γ - η - γ) [1..]

expected' :: [Double] -- η = γ
expected' = map (\π -> 1 + 2/π^2 - 2/π) [1..]

guess :: Int -> Int -> IO Double
guess η γ = do
   event <- sample η
   guess <- sample γ
   let right = filter (== True) $ zipWith (==) guess event
   let ratio = fromIntegral (length right) / fromIntegral s
   pure ratio

sample :: Int -> IO [Bool]
sample x = do
   gen <- newStdGen
   pure . take s . map (== 1) . randomRs (1,x) $ gen

