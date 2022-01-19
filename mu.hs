main = print $ take 10 $ y (1:)

newtype Mu a = Mu (Mu a -> a)

y :: (a -> a) -> a
y f = (\x -> f (x (Mu x))) (\(Mu x) -> f (x (Mu x)))

