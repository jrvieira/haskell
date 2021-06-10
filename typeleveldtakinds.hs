{-# LANGUAGE DataKinds , StandaloneKindSignaures , GADTs #-}

import Data.Kind (Type)

main :: IO ()
main = do
   print $ "ok"

data Nat = Zero | Succ Nat


