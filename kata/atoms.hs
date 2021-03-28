module MoleculeToAtoms where

import Control.Monad ((>=>))

parseMolecule :: String -> Either String [(String,Int)]
parseMolecule = parse

(>==) :: (a -> Either l r) -> (r -> b) -> a -> Either l b
a >== b = either Left (Right . b) . a

parse = symbols >=> brackets >=> validator >=> tokens >=> lexemes >== lexic >== atoms

-- Symbols

upper :: Char -> Bool
upper = flip elem ['A'..'Z']
lower :: Char -> Bool
lower = flip elem ['a'..'z']
numbr :: Char -> Bool
numbr = flip elem ['0'..'9']
openr :: Char -> Bool
openr = flip elem "([{"
closr :: Char -> Bool
closr = flip elem ")]}"

data Symbol = Upper Char | Lower Char | Numbr Char | Openr Char | Closr Char deriving (Show, Eq)

symbols :: String -> Either String [Symbol]
symbols [] = Right []
symbols (x:xx)
   | upper x = (Upper x :) <$> symbols xx
   | lower x = (Lower x :) <$> symbols xx
   | numbr x = (Numbr x :) <$> symbols xx
   | openr x = (Openr x :) <$> symbols xx
   | closr x = (Closr x :) <$> symbols xx
   | otherwise = Left $ "illegal char" ++ [x]

isOpenr :: Symbol -> Bool
isOpenr s
   | Openr _ <- s = True
   | otherwise    = False

isClosr :: Symbol -> Bool
isClosr s
   | Closr _ <- s = True
   | otherwise    = False

-- Validator

brackets :: [Symbol] -> Either String [Symbol]
brackets s
   | balanced s' && matching s' = Right s
   | otherwise                  = Left "unmatched brackets"
      where
         s' = strip s

balanced :: [Symbol] -> Bool
balanced ss = length (filter isOpenr ss) == length (filter isClosr ss)
matching :: [Symbol] -> Bool
matching = null.reduce

reduce :: [Symbol] -> [Symbol]
reduce s
   | u == uu = u
   | otherwise      = reduce $ uu
      where
         u = unbracket s
         uu = unbracket u
         unbracket [] = []
         unbracket [s] = [s]
         unbracket (s:ss @ (s':ss'))
            | Openr o <- s, Closr c <- s', match o c = unbracket ss'
            | otherwise               = s : unbracket ss
               where
                  match l r
                     | '(' <- l, ')' <- r = True
                     | '[' <- l, ']' <- r = True
                     | '{' <- l, '}' <- r = True
                     | otherwise          = False

strip :: [Symbol] -> [Symbol]
strip [] = []
strip (s:ss)
   | Openr _ <- s = s : strip ss
   | Closr _ <- s = s : strip ss
   | otherwise  = strip ss

validator :: [Symbol] -> Either String [Symbol]
validator [] = Right []
validator [s]
  | Openr _ <- s = Left "Unexpected EOS"
  | Closr _ <- s = Left "Unexpected EOS"
  | otherwise  = Right [s]
validator (s:ss @ (s':ss'))
  | Lower _ <- s', Upper _ <- s = (s :) <$> validator ss
  | Lower _ <- s', _       <- s = Left "Lower without Upper"
  | otherwise                   = (s :) <$> validator ss

-- Tokens

data Token = Element String | Number String | Open | Close deriving Show

tokens :: [Symbol] -> Either String [Token]
tokens = tokens' []
   where
      tokens' :: [Token] -> [Symbol] -> Either String [Token]
      tokens' tks [] = Right tks
      tokens' tks @ ~(t:tt) (s:ss)
         | Upper c <- s                   = tokens' (Element [c] : tks) ss
         | Lower c <- s , Element c' <- t = tokens' (Element (c'++[c]) : tt) ss
         | Numbr c <- s , Number  c' <- t = tokens' (Number (c'++[c]) : tt) ss
         | Numbr c <- s                   = tokens' (Number [c] : tks) ss
         | Openr _ <- s                   = tokens' (Open : tks) ss
         | Closr _ <- s                   = tokens' (Close : tks) ss
         | otherwise                      = Left $ "unexpected symbol" ++ show s

-- mind that [Token] is reversed in relation to input

-- Lexemes

data Lexeme = Elem Int String | Mult Int | Over deriving Show

lexemes :: [Token] -> Either String [Lexeme]
lexemes [] = Right []
lexemes (x:xx @ ~(x':xx'))
   | Number  s <- x , Close      <- x' = (Mult (read s) :) <$> lexemes xx'
   | Number  s <- x , Element s' <- x' = (Elem (read s) s' :) <$> lexemes xx'
   | Element s <- x                    = (Elem 1 s :) <$> lexemes xx
   | Open      <- x                    = (Over :) <$> lexemes xx
   | Close     <- x                    = (Mult 1 :) <$> lexemes xx
   | otherwise                         = Left $ "unexpected token" ++ show x

data Lexic = Node Int Lexic Lexic | Atom String | Leaf

instance Show Lexic where
   show (Node i l next)
      | Atom _ <- l = show l ++ show i ++ " " ++ show next
      | otherwise   = show i ++ "[ " ++ show l ++ " ] " ++ show next
   show Leaf = "*"
   show (Atom s) = s

lexic :: [Lexeme] -> Lexic
lexic = fst.lexic'
   where
      lexic' :: [Lexeme] -> (Lexic,[Lexeme])
      lexic' [] = (Leaf,undefined)
      lexic' (l:ll)
         | Elem i s <- l = (Node i (Atom s) node,rest)
         | Mult i   <- l = (Node i node node',rest')
         | Over     <- l = (Leaf,ll)
            where
               (node ,rest ) = lexic' ll
               (node',rest') = lexic' rest

-- Counter

type Quant = (String,Int)
type Total = [Quant]

count :: Lexic -> Total
count Leaf = []
count (Atom s) = [(s,1)]
count (Node i l next)
   | Leaf <- l = []
   | Atom s <- l = (s,i) : count next
   | Node {} <- l = map ((*i)<$>) (count l) ++ count next

add :: Total -> Quant -> Total
add [] qnt = [qnt]
add (q @ (s,i):qq) (q' @ (s',i'))
   | s == s' = add qq (s,i+i')
   | otherwise = q : add qq q'

flatten :: Total -> Total
flatten [] = []
flatten (q:qq) = add (flatten qq) q

atoms :: Lexic -> Total
atoms = flatten.count
