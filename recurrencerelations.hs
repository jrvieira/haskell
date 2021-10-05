{-# LANGUAGE TupleSections, ScopedTypeVariables #-}

import Data.Traversable ( mapAccumL )
import Data.Map.Lazy ( Map )
import qualified Data.Map.Strict as M
import Test.Hspec

main :: IO ()
main = hspec spec

ρ :: forall a b. Ord a => (a -> Either b ([a],[b] -> b)) -> a -> b
ρ f = snd . go M.empty
   where
   go :: Map a b -> a -> (Map a b,b)
   go m n = maybe (either (m,) fo (f n)) (m,) (M.lookup n m)
      where
      fo :: ([a],[b] -> b) -> (Map a b,b)
      fo (l,f') = memo $ f' <$> mapAccumL go m l
      memo (m',x) = (M.insert n x m',x)

{- https://www.codewars.com/kata/550756a881b8bdba99000348/train/haskell

   Say we have a function of this type:

   f :: Ord a => a -> Either b ([a], [b] -> b)

   Any recursive function of one argument can be implemented
   in such a way that it fits this type signature - provided
   with an argument it returns either the result (Left-case)
   or a pair of new arguments for the function and a function
   to fold the results (Right-case). For example:


   factorial i | i == 0    = Left 1 | otherwise = Right ([i-1], (*i).head)

   fibonacci i | i < 2     = Left i
               | otherwise = Right ([i-1, i-2], sum)


   This gives us the usual factorial and Fibonacci functions
   (for more examples, see the test cases)

   Your task is to write an evaluator for such functions,
   i.e a function:

   ρ :: Ord a => (a -> Either b ([a], [b] -> b)) -> a -> b

   This function takes a function of a previously described
   form and turns it into a simple a -> b function.

-}

factorial i | i == 0    = Left 1
            | otherwise = Right ([i-1], (*i).head)

fibonacci i | i < 2     = Left i
            | otherwise = Right ([i-1, i-2], sum)

coinchange (a, i) | a == 0          = Left 1
                  | a < 0 || i == 0 = Left 0
                  | otherwise       = Right ([(a, i-1), (a-coinlist!!(i-1), i)], sum)
coinlist = [1, 3, 5, 10]

heigth (n, m) | m <= 0 || n <= 0 = Left 0
              | otherwise        = Right ([(n, m-1), (n-1, m-1)], (+1).sum)

foo  i | i <= 2    = Left 1
       | odd i     = Right ([6*i`div`7, 2*i`div`3], sum)
       | otherwise = Right ([i-1, i-3], sum)

spec :: Spec
spec = do
  describe "FunctionEvaluator" $ do
    it "should work for some basic tests" $ do
      ρ factorial 5    `shouldBe` 120
      ρ factorial 20   `shouldBe` 2432902008176640000
      ρ fibonacci 10   `shouldBe` 55
      ρ coinchange (20,  length coinlist) `shouldBe` 28
      ρ heigth (2, 14) `shouldBe` 105
      ρ foo 20         `shouldBe` 253

    it "should work for some advanced tests" $ do
      ρ fibonacci 100     `shouldBe` 354224848179261915075
      ρ coinchange (500, length coinlist) `shouldBe` 146948
      ρ heigth (100, 150) `shouldBe` 1427228946471605830963606579751332537625641011
      ρ foo (10^12-3)     `shouldBe` 10393063677856661930403634886614144354995461022573498
