main = do
    print $ Some A  -- :: Will Alpha
    print $ Some B  -- :: Will Beta
    print $ None    -- :: Will ()

data Alpha = A deriving Show
data Beta = B deriving Show

class Willable a

instance Willable Alpha
instance Willable Beta

data Will a = Willable a => Some a | a ~ () => None

-- same as:

-- data Will a where
--    Some :: Willable a => a -> Will a
--    None :: Will ()

deriving instance Show a => Show (Will a)