fac 0 = 1
fac n = n * (fac $ n - 1)

c n r | r <= n = (fac n) `div` ((fac r) * (fac $ n - r))

euler53' = [(n, r, c n r) | n <- [1..100], r <- [1..n],
            (c n r) > 1000000]

euler53 = length euler53'
            