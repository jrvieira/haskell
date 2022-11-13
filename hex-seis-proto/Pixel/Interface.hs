module Pixel.Interface where

import Data.Type
import Data.Verse

import System.IO
import System.Console.ANSI

   {-

   Keyboard movement control:

           U           I                   K
   o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o
     *   *   *   *   *   *   *   *   *   *   *   *   *   *   *
   o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o
   *   *   *   u   i   *   *   *   *   *   *   k   *   *   *   *
   o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o
     H   *   h   j   k   *   K   *   H   *   h   i   l   *   L
   o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o
   *   *   *   n   m   *   *   *   *   *   *   *   j   *   *   *
   o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o
     *   *   *   *   *   *   *   *   *   *   *   *   *   *   *
   o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o
           N           M                               J
                                             [Vi mode]

   -}

{-
cqx = trap (flip elem "cqx") 
cqx >>= \a -> cqx >>= \b -> cqx >>= \c -> pure [a,b,c]
-- returns 3 pressed (c|q|x) chars
-}
trap :: (Char -> Bool) -> IO Char
trap p = getChar >>= go
   where
   go c
      | p c = pure c
      | otherwise = trap p

silent :: IO a -> IO a
silent io = do
   inB <- hGetBuffering stdin
   outB <- hGetBuffering stdout
   inE <- hGetEcho stdin
   outE <- hGetEcho stdout
   hSetBuffering stdin NoBuffering
   hSetBuffering stdout NoBuffering
   hSetEcho stdout True
   hSetEcho stdin False
   hSetEcho stdout False
   r <- io
   hSetBuffering stdin inB
   hSetBuffering stdout outB
   hSetEcho stdout inE
   hSetEcho stdin outE
   pure r

step :: (Universe -> IO ()) -> Universe -> IO Universe
step f u = clearScreen >> f u >> go
   where
   go = do
      a <- silent $ trap (flip elem "hjkl")
      b <- silent $ trap (== a)
      if elem [a,b] commands then step f $ move [a,b] u else go

commands :: [[Char]]
commands = ["kk","ll","jj","hh"]

move :: [Char] -> Universe -> Universe
move "kk" u = u { ι = (ι u) { φ = φ (ι u) <> Some (-4*w) } }
move "ll" u = u { ι = (ι u) { φ = φ (ι u) <> Some   2    } }
move "jj" u = u { ι = (ι u) { φ = φ (ι u) <> Some ( 4*w) } }
move "hh" u = u { ι = (ι u) { φ = φ (ι u) <> Some (-2  ) } }
move _ _ = error "deficient mapping"

