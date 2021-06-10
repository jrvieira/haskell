import Control.Concurrent

main :: IO ()
main = do
   print $ "ok"
   threadA <- forkIO ioA
   threadB <- forkIO ioB
   mapM_ print [threadA,threadB]
   pure ()

ioA :: IO ()
ioA = putStr "_______"

ioB :: IO ()
ioB = putStr "......."

