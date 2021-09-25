{-# LANGUAGE OverloadedStrings #-}

module LinearFeedbackShiftRegister where

-- import Data.ByteString.Lazy

main :: IO ()
main = do
-- print $ bits "1001"
-- print $ xor (bits "1001") (bits "1010")
-- print $ shl (bits "1001")
-- print $ shr (bits "1001")
   mapM_ print $ take 16 lfsr
   putStrLn ""
   mapM_ print $ take 16 lfsr403
   putStrLn ""
   mapM_ print $ take 16 $ zipWith xor lfsr lfsr403
   putStrLn ""
   mapM_ print $ take 16 $ lfsr0011

data Bits = Nil | Bit Bool Bits

instance Show Bits where
   show Nil = []
   show (Bit b bs) = (if b then '#' else '-') : show bs

bits "" = Nil
bits (c:cs)
   | '1' <- c = Bit True  $ bits cs
   | '0' <- c = Bit False $ bits cs
   | otherwise = error $ "invalid bit: " ++ [c]

(!) :: Bits -> Int -> Bool
Nil ! _ = error "empty Bits"
(Bit b bs) ! n
   | n < 0 = error "negative index"
   | n == 0 = b
   | otherwise = bs ! pred n

combine :: (Bool -> Bool -> Bool) -> Bits -> Bits -> Bits
combine _ Nil Nil = Nil
combine f (Bit a as) (Bit b bs) = Bit (f a b) (combine f as bs)
combine _ _ _ = error "unequal length combine"

xor :: Bits -> Bits -> Bits
xor = combine (/=)

and :: Bits -> Bits -> Bits
and = combine (&&)

or :: Bits -> Bits -> Bits
or = combine (||)

-- right shift
shr :: Bits -> Bits
shr Nil = Nil
shr bits = Bit False (go bits)
   where
   go Nil = Nil
   go (Bit b bs)
      | Nil <- bs = Nil
      | otherwise = Bit b (go bs)

-- left shift
shl :: Bits -> Bits
shl Nil = Nil
shl (Bit _ bs) = go bs
   where
   go Nil = Bit False Nil
   go (Bit b bs) = Bit b (go bs)

lfsr :: [Bits]
lfsr = iterate go $ bits "1001"
   where
   go b = Bit b0 bt
      where
      (Bit _ bt) = shr b
      b0 = b ! 2 /= b ! 3

lfsr403 :: [Bits]
lfsr403 = iterate go $ bits "1001"
   where
   go b = Bit b0 bt
      where
      (Bit _ bt) = shr b
      b0 = b ! 0 /= b ! 3

lfsr0011 :: [Bits]
lfsr0011 = iterate go $ bits "0011"
   where
   go b = Bit b0 bt
      where
      (Bit _ bt) = shr b
      b0 = b ! 2 /= b ! 3

