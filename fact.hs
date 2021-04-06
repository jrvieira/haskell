(!) :: Int -> a -> Int
n ! _
    | n  < 1 = error "negative factorial"
    | n == 1 = 1
    | n  > 1 = n * ((n-1) ! ())

main = print $ 6 ! ()
