module Zero.Color where

data Color = Black | Red | Green | Yellow | Blue | Purple | Cyan | White | None | Grey | LRed | LGreen | LYellow | LBlue | LPurple | LCyan | LWhite

instance Enum Color where
   fromEnum Black = 30
   fromEnum Red = 31
   fromEnum Green = 32
   fromEnum Yellow = 33
   fromEnum Blue = 34
   fromEnum Purple = 35
   fromEnum Cyan = 36
   fromEnum White = 37
   fromEnum None = 39
   fromEnum Grey = 90
   fromEnum LRed = 91
   fromEnum LGreen = 92
   fromEnum LYellow = 93
   fromEnum LBlue = 94
   fromEnum LPurple = 95
   fromEnum LCyan = 96
   fromEnum LWhite = 97
   toEnum 30 = Black
   toEnum 31 = Red
   toEnum 32 = Green
   toEnum 33 = Yellow
   toEnum 34 = Blue
   toEnum 35 = Purple
   toEnum 36 = Cyan
   toEnum 37 = White
   toEnum 39 = None
   toEnum 90 = Grey
   toEnum 91 = LRed
   toEnum 92 = LGreen
   toEnum 93 = LYellow
   toEnum 94 = LBlue
   toEnum 95 = LPurple
   toEnum 96 = LCyan
   toEnum 97 = LWhite
   toEnum _ = None

clr :: Color -> Color -> String -> String
clr fg bg s = "\x1b[" <> fg' <> ";" <> bg' <> "m" <> s <> "\x1b[0m"
   where
   fg' = show $ fromEnum fg
   bg' = show $ fromEnum bg + 10

