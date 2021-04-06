module Main where

import Sim

main :: IO ()
main = run tapestry

tapestry = concat $ midr h (take w (repeat '.')) (midr w '.' 'x') -- 2x2 square in the middle

midr n normal middle
   | 0 <- mod n 2 = take half blank ++ mid ++ take half blank
   | 1 <- mod n 2 = take half blank ++ mid ++ take (half+1) blank
      where
         half = div n 2 - 1
         blank = repeat normal
         mid = take 2 $ repeat middle


conway = "\
   \.........................\
   \.........................\
   \.........................\
   \...........x.............\
   \..........xxx............\
   \.........xxxxx...........\
   \.........................\
   \.........................\
   \.........................\
   \.........................\
   \.........................\
   \.........................\
   \.........xxxxx...........\
   \..........xxx............\
   \...........x.............\
   \.........................\
   \.........................\
   \.........................\
   \"

random = "\
   \..xx.xx.xx.xx.xx.xx.xx.xx\
   \...x.xx..xxx.xx.x.x......\
   \xx.x.xxxxx.xx..x.xx......\
   \.....xxxx.x...x..xxx.....\
   \xx..xxx...xxx....x.xx.xx.\
   \.........xxxxx...........\
   \.x.xx.x..x.xx.x..x.xx..x.\
   \.........................\
   \x.x.xx..x..xx..x.xx.x....\
   \......x..x.x.x.x.xx.x..x.\
   \..x.x..x.xx..............\
   \.........x.xx.x..x..x..x.\
   \..xxx.x.xxx.xx.xxxxx.xx..\
   \..........xxx............\
   \...........x.............\
   \xxx.xx.x.xxx.xxx.xxx.xx..\
   \.....xxxxxxxxxxxxx.......\
   \.x.x.x..x.x.x.x.x.xxx.xx.\
   \"
