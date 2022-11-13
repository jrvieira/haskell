-- proof: digital root = mod (base-1)

-- c * n^2 + b * n^1 + a * n^0
-- cnn + bn + a
-- c * nn-1 + c + b * n-1 + b + a
-- c * nn-1 + b * n-1 + c + b + a
-- proof
-- n-1 * something + c + b + a
-- QED
--
-- n^x - 1 mod n = n-1 :)
-- n^x - 1 is divisible by n-1 (trust me or proof)
--
-- nn-1 /= n(n-1)

main = do
   print $ and $ modpn <$> take 4 [2..] <*> take 4 [1..]
   print $ and $ proof <$> take 4 [2..] <*> take 4 [1..]

modpn n x = mod (n^x - 1)  n      == n - 1
proof n x = rem (n^x - 1) (n - 1) == 0
