import Data.List (sort)

main :: IO ()
main = do
   print mults
   mapM_ print $ sort mults

data Mult = Mult { β :: Int , μ :: Int , ρ :: Int }

mult a b = Mult a b (a*b)

instance Eq Mult where
   a == b = (ρ a) == (ρ b)

instance Show Mult where
   show (Mult b m r) = show b <> " * " <> show m <> " = " <> show r

instance Ord Mult where
   compare a b = compare (ρ a) (ρ b)

mults :: [Mult]
mults = mult <$> [1..7] <*> [1..7]

