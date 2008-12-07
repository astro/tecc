hoch n i | i == 1 = n
         | i > 1 = n * hoch n i'
    where i' = i - 1

digits n = length $ show n

euler_63' b n = digits e == n
    where e = b `hoch` n

euler_63 = [(b,n,b `hoch` n) | b <- [1..100], n <- [1..100], euler_63' b n]