fibo_seq' a b = c:rest
    where c = a + b
          rest = fibo_seq' b c

fibo_seq = 1:2:(fibo_seq' 1 2)

euler_2 = sum $
          takeWhile (\x -> x < 4000000) $
                    filter (\x -> x `rem` 2 == 0) $
                           fibo_seq
