import Data.Maybe
import Data.IntMap.Strict hiding (null,map,foldr)

-- goblets tic tac toe

{- QUESTION:
   is there always a play
   that guarantees a win?
-}

main :: IO ()
main = do
   let game = tree start
   let w = length $ wins game
   let t = length $ ties game
   let e = length $ ends game
   print $ [w,t,e]
   print $ w+t == e

-- setup

instance Show State where
   show s = "\n" ++ a ++ " | " ++ b ++ " | " ++ c ++ "\n" ++ d ++ " | " ++ e ++ " | " ++ f ++ "\n" ++ g ++ " | " ++ h ++ " | " ++ i ++ "\n"
      where
      [a,b,c,d,e,f,g,h,i] = map (draw . snd) $ toList $ β s
      draw Nothing = " "
      draw (Just p) = k (κ p) ++ show (γ p) ++ "\ESC[0m"
      k 0 = "\ESC[0;31m"
      k 1 = "\ESC[0;32m"
      k _ = "?"

data State = Γ { τ :: Color , β :: Board , π :: Hands }
type Hands = IntMap (IntMap Int) -- colors, pieces
type Board = IntMap (Maybe Piece)
data Piece = P { γ :: Grade , κ :: Color }
   deriving Eq
type Grade = Int
type Color = Int
type Place = Int

seats :: Int
seats = 2

hands :: IntMap Grade
hands = fromList $ zip [0..] [2,2,2]

start :: State
start = Γ 0 b h
   where
   b = fromList $ zip [0..] (take 9 $ repeat Nothing)
   h = fromList $ zip [0..] (replicate seats hands)

-- interface

-- Nothing => invalid move
move :: Piece -> Place -> State -> Maybe State
move p i (Γ t b h)
   | tops p (b ! i) = Just (Γ t' b' h')
   | otherwise = Nothing
   where
   h' = adjust (update play (γ p)) (κ p) h
   b' = insert i (Just p) b
   t' = mod (succ t) seats

tops :: Piece -> Maybe Piece -> Bool
tops p m
   | Nothing <- m = True
   | Just jp <- m = γ p > γ jp

play :: Int -> Maybe Int
play i
   | i' > 0 = Just i'
   | otherwise = Nothing
   where
   i' = pred i

-- mechanics

data Tree = Node State [Tree] | Over State (Maybe Color)

tree :: State -> Tree
tree s 
   | null ss = Over s Nothing -- no more possible moves
   | Right mk <- over s = Over s mk -- Right Nothing => pieces exhausted
   | otherwise = Node s (tree <$> ss)
   where
   ss = step s

ends :: Tree -> [State]
ends t = go t []
   where
   go (Node _ ss) = flip (foldr go) ss
   go (Over s _) = (s :)

wins :: Tree -> [State]
wins t = go t []
   where
   go (Node _ ss) = flip (foldr go) ss
   go (Over s (Just _)) = (s :)
   go _ = id

ties :: Tree -> [State]
ties t = go t []
   where
   go (Node _ ss) = flip (foldr go) ss
   go (Over s Nothing) = (s :)
   go _ = id

step :: State -> [State]
step s@(Γ t b h) = catMaybes [move (P g t) i s | g <- keys (h ! t) , i <- take 9 [0..]]

-- Left () => not over
-- Right Nothing => draw
-- Right (Just k) => win k
over :: State -> Either () (Maybe Color)
over g
   | all null $ π g = Right Nothing
   | [x,y,z,_,_,_,_,_,_] <- b , x /= Nothing , (κ <$> x) == (κ <$> y) , (κ <$> y) == (κ <$> z) = Right (κ <$> x)
   | [_,_,_,x,y,z,_,_,_] <- b , x /= Nothing , (κ <$> x) == (κ <$> y) , (κ <$> y) == (κ <$> z) = Right (κ <$> x)
   | [_,_,_,_,_,_,x,y,z] <- b , x /= Nothing , (κ <$> x) == (κ <$> y) , (κ <$> y) == (κ <$> z) = Right (κ <$> x)
   | [x,_,_,y,_,_,z,_,_] <- b , x /= Nothing , (κ <$> x) == (κ <$> y) , (κ <$> y) == (κ <$> z) = Right (κ <$> x)
   | [_,x,_,_,y,_,_,z,_] <- b , x /= Nothing , (κ <$> x) == (κ <$> y) , (κ <$> y) == (κ <$> z) = Right (κ <$> x)
   | [_,_,x,_,_,y,_,_,z] <- b , x /= Nothing , (κ <$> x) == (κ <$> y) , (κ <$> y) == (κ <$> z) = Right (κ <$> x)
   | [x,_,_,_,y,_,_,_,z] <- b , x /= Nothing , (κ <$> x) == (κ <$> y) , (κ <$> y) == (κ <$> z) = Right (κ <$> x)
   | [_,_,x,_,y,_,z,_,_] <- b , x /= Nothing , (κ <$> x) == (κ <$> y) , (κ <$> y) == (κ <$> z) = Right (κ <$> x)
   | otherwise = Left ()
   where
   b = map snd $ toList $ β g

