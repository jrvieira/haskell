main = print $ take 10 $ y (1:)

{- Recursive type Mu guides the typechecking of the Y combinator
   otherwise (\f -> (\x -> f (x x)) (\x -> f (x x))) fails with
   error:
    • Occurs check: cannot construct the infinite type: a0 ~ a0 -> b
      Expected type: (a0 -> b) -> (a0 -> b) -> b
-}

newtype Mu a = Mu (Mu a -> a)

y :: (a -> a) -> a
y f = (\x -> f (x (Mu x))) (\(Mu x) -> f (x (Mu x)))

