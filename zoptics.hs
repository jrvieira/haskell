main :: IO ()
main = do
   let t = T 6 'w'
   print $ tx succ $ ty pred $ t
   print $ txy succ pred $ t

   let u = U 6 'w'
   print $ ux succ $ uy pred $ u
   print $ uxy succ pred $ u

-- T

data T = T { _x :: Int , _y :: Char }
   deriving Show

tx f t@T { _x = v } = t { _x = f v }
ty f t@T { _y = v } = t { _y = f v }

txy fx fy = tx fx . ty fy

-- U

data U = U Int Char
   deriving Show

ux f (U i c) = U (f i) c
uy f (U i c) = U i (f c)

uxy fx fy = ux fx . uy fy
