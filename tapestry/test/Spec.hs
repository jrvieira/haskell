module Main where

import Sim
import Shut

main :: IO ()
main = test [
   unit "alpha" (render . next . load $ "x...") ".xx.",
   unit "beta" (render . next . load $ "xx..") "xxxx",
   unit "gamma" (render . next . load $ "xxx.") ".xx.",
   unit "delta" (render . next . load $ ".xx.") "....",
   unit "epsilon" (render . next . load $ "xxxx") "...."
   ]
