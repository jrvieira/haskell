module Exo (run) where

import Coord

import Data.List.Split
import System.Console.ANSI
import Control.Concurrent

run :: IO ()
run = mapM_ play buffer
   where
      play s = do
         --seq screen
         (threadDelay $ div 1000000 fps)
         --clearScreen
         putStrLn s
         pure ()

screen :: Int -> String
screen i = unlines . chunksOf w
   . mark (rel Id i) 'η'
   . mark (rel Alpha i) 'α'
   . mark (rel Beta i) 'β'
   . mark (rel Gamma i) 'γ'
   . mark (rel Delta i) 'δ'
   . mark (rel Epsilon i) 'ε'
   . mark (rel Zeta i) 'ζ'
   $ list
   where
      list = take s $ cycle "."

buffer :: [String]
buffer = map screen $ [0..s]

mark :: Int -> Char -> String -> String
mark n c s = take n s ++ [c] ++ drop (n + 1) s
