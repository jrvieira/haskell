dots :: Integer -> String
dots 0 = ""
dots 1 = "."
dots n = dots (n-1) ++ "."

-- p

parity :: Integer -> Integer
parity n
    | n < 2 = 0
    | mod n 2 == 1 = 0
    | mod n 2 == 0 = parity (div n 2) + 1

prank :: Integer -> String
prank n = show n ++ " " ++ show (parity n) ++ " " ++ (dots (parity n))

p :: [Integer] -> IO ()
p l = mapM_ putStrLn $ prank <$> l

-- t

tarity :: Integer -> Integer
tarity n
    | n < 2 = 0
    | mod (n - 1) 3 == 0 = tarity (div (n-1) 3) + 1
    | mod n 2 == 0 = parity (div n 2) + 1
    | n > 1 = 0

trank :: Integer -> String
trank n = show n ++ " " ++ show (tarity n) ++ " " ++ (dots (tarity n))

t :: [Integer] -> IO ()
t l = mapM_ putStrLn $ trank <$> l

-- x

xarity :: Integer -> Integer
xarity n
    | n < 2 = 0
    | mod (n - 1) 3 == 0 = xarity (div (n-1) 3) + 1
    | mod n 2 == 0 = xarity (div n 2) + 1
    | n > 1 = 0

xrank :: Integer -> String
xrank n = show n ++ " " ++ show (xarity n) ++ " " ++ (dots (xarity n))

x :: [Integer] -> IO ()
x l = mapM_ putStrLn $ xrank <$> l

-- c

carity :: Integer -> Integer
carity n
    | n == 1 = 0
    | mod n 2 == 0 = carity (div n 2) + 1
    | mod n 2 == 1 = carity (n*3 + 1) + 1

crank :: Integer -> String
crank n = show n ++ " " ++ show (carity n) ++ " " ++ (dots (carity n))

c :: [Integer] -> IO ()
c l = mapM_ putStrLn $ crank <$> l
