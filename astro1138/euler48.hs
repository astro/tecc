hoch n 1 = n
hoch n e = n * hoch n e'
    where e' = e - 1

euler_48 = sum $ [n `hoch` n | n <- [1..1000]]
