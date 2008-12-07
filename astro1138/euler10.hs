eliminate' _ [] _ = []
eliminate' delta (e:list) n | e < n = e:(eliminate' delta list n)
                            | e == n = eliminate' delta list n
                            | e > n = eliminate' delta (e:list) (n + delta)
eliminate delta list = eliminate' delta list delta

prime_sieve [] = []
prime_sieve (n:rest) = n:rest''
    where rest' = eliminate n rest
          rest'' = prime_sieve rest'

euler_10 = (primes, s)
    where s = sum primes
          primes = prime_sieve [2..2000000]

main = do
  putStrLn $ show $ euler_10
