{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Pixel where

data GD = FG | BG | FG' | BG'

instance Enum GD where
   fromEnum FG = 30
   fromEnum BG = 40
   fromEnum FG' = 90
   fromEnum BG' = 100
   toEnum 30 = FG
   toEnum 40 = BG
   toEnum 90 = FG'
   toEnum 100 = BG'

data Color = K | R | G | Y | B | M | C | W | C8Bit | N
   deriving (Eq,Enum,Bounded)

instance Semigroup Color where
   a <> N = a
   N <> b = b
   _ <> b = b

instance Monoid Color where
   mempty = N

instance Show Color where
   show c = clr BG c "  "

newtype Pixel = Pixel { κ :: Color }
   deriving (Semigroup,Monoid)

instance Show Pixel where
   show (Pixel N) = clr FG K "+ "
   show (Pixel k) = clr FG k "o "

sgr :: Int -> String
sgr i = "\ESC[" <> show i <> "m"

set :: GD -> Color -> String
set gd c = sgr $ fromEnum gd + fromEnum c

reset :: String
reset = sgr 0

clr :: GD -> Color -> String -> String
clr gd c s = set gd c <> s <> reset

-- 8 bit - 6 x 6 x 6 cube = 256 colors

data Color8 = Color8 Int

instance Show Color8 where
   show (Color8 c) = clr8 BG c "  "

clr8 :: GD -> Int -> String -> String
clr8 gd c s = "\ESC[" <> show x <> ";5;" <> show c <> "m" <> s <> reset
   where
   x = fromEnum gd + 8

clr8rgb :: GD -> (Int,Int,Int) -> String -> String
clr8rgb gd (r,g,b) s = "\ESC[" <> show x <> ";5;" <> show c <> "m" <> s <> reset
   where
   c = 16 + 36 * r + 6 * g + b
   x = fromEnum gd + 8

-- colorSpace8 :: [Int]
-- colorSpace8 = [0..255]

-- colorSpace8rgb :: [(Int,Int,Int)]
-- colorSpace8rgb = (,,) <$> [0..5] <*> [0..5] <*> [0..5]

-- mapM_ putStr $ flip (clr BG) "  " <$> [minBound..maxBound]
-- mapM_ putStr $ flip (clr8 BG) "  " <$> colorSpace8
-- mapM_ putStr $ flip (clr8rgb BG) "  " <$> colorSpace8rgb

