import Numeric.Natural
import qualified Lib.Module as M (method)

-- Primes
is_prime :: Natural -> Bool
is_prime n = null [d | d <- [2..n-1] , mod n d == 0] -- no divisors between 2 and n-1

primes :: [Natural]
primes = [n | n <- [2..] , is_prime n]
-- == primes = [n | n <- [2..] , null [d | d <- [2..n-1] , mod n d == 0]]

p = take 8 primes

-- Binary

--vbin b i = [2^n | n <- [0..], i > 2^n]
-- b = number of bits

bin :: Int -> Natural -> [Char]
bin b i
    | i == 0 = replicate b '0' -- opt
    | i < n = '0' : bin (b-1) i
    | i >= n = '1' : bin (b-1) (i-n)
--    | i == n = 1 : replicate (b-1) 0 -- opt
    | otherwise = []
    where n = 2^(b-1)

zbin :: Natural -> [Natural]
zbin i
    | i == 0 = []
    | i > 0 = mod i 2 : zbin (div i 2)

zbase :: Natural -> Natural -> [Natural]
-- b = base
zbase b i
    | i <= 0 = []
    | i > 0 = mod i b : zbase b (div i b)

base :: Natural -> Natural -> [Natural]

-- Kleene trinary logic (3VL)

data Tri = V | M | F
    deriving (Show, Eq)

(#) :: Tri -> Tri -> Tri
(#) a b
    | a == b = a
    | elem F [a, b] = F
    | otherwise = M

-- Rock Paper Scisors

base b = reverse . zbase b
data El = R | P | S deriving (Show, Eq)

class Game e where
    (##) :: e -> e -> Maybe e

instance Game El where
    R ## S = Just R
    P ## R = Just P
    S ## P = Just S
    a ## b = if a == b then Nothing else b ## a

-- Comparables

data Is a = No | Yes a
    deriving Show

type Comparable a = Is a -> Is a -> Is a

(.>) :: Ord a => Comparable a
(.>) (Yes a) (Yes b) = if a > b then (Yes b) else No
(.>) _ _ = No

(.<) :: Ord a => Comparable a
(.<) (Yes a) (Yes b) = if a < b then (Yes b) else No
(.<) _ _ = No

(-=) :: Eq a => Comparable a
(-=) (Yes a) (Yes b) = if a == b then (Yes b) else No
(-=) _ _ = No


-- Singly linked list
data Link a = Nil | Node a (Link a)

list :: Link a -> [a]
list Nil = []
list (Node a link) = a : list link

instance Show a => Show (Link a) where
    show = show . list

--ls = Node 'a' (Node 'b' (Node 'c' (Nil)))

-- Logic gates

type Gate = Bool -> Bool -> Bool

nand' :: Gate
nand' True True = False
nand' _ _ = True

inv' :: Bool -> Bool
inv' x = nand' x x

and' :: Gate
and' x y = inv' $ nand' x y

or' :: Gate
or' x y = nand' (inv' x) (inv' y)

nor' :: Gate
nor' x y = inv' $ or' x y

xor' :: Gate
xor' x y =  nand' (nand' x (nand' x y)) (nand' (nand' x y) y)

nxor' :: Gate
nxor' x y = inv' $ xor' x y


table :: Gate -> String
table f = tt ++ tf ++ ff where
    tt = if (f True True) then "T" else "F"
    tf = if (f True False) then "T" else "F"
    ff = if (f False False) then "T" else "F"


{- TODO

-}

f :: m a -> m a
f = id

-- f' :: m a -> m b
-- f' a = a

rnd :: IO ()
rnd = do
    line <- getLine
    print $ read "9":: Int
    pure ()
