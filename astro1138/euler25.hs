fibo_seq' n a b = (n,a):f'
    where f' = fibo_seq' n' b c
          c = a + b
          n' = n + 1
fibo_seq = fibo_seq' 1 1 1

hoch n 1 = n
hoch n e = n * hoch n e'
    where e' = e - 1

limit = 10 `hoch` 999

euler25 = head $ filter (\(i,n) -> n >= limit) $ fibo_seq