import System.Environment
import Debug.Trace

ack :: Int -> Int -> Int
ack 0 n = n+1
ack m 0 = trace (show m ++ " 0") $ ack (m-1) 1
ack m n = trace (show m ++ " " ++ show n) $ ack (m-1) (ack m (n-1))

main :: IO ()
main = do
   args <- getArgs
   if length args == 2 then let
      (m:n:[]) = map read args
      in
      print $ ack m n
   else do
      error "missing arguments @ ack m n"


