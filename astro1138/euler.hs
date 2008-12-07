import List (foldl)

is_prime 1 = False
is_prime 2 = True
is_prime x = not (any (\n -> x `rem` n == 0) [2..(x `div` 2)])

primes = [x | x <- [1..], is_prime x]

primes_until n = takeWhile (<= n) primes

{-
sieve :: [Int] -> [Int] -> [Int]
sieve [] result = result
sieve (n:t) result = let t2 = (filter (\ m ->
                                           m `rem` n /= 0)
                               t)
                     in
                       sieve t2 (n:result)
-}
sieve :: [Int] -> [Int]
sieve [] = []
sieve (n:t) = let t2 = [m | m <- t, m `rem` n /= 0]
              in
                n:(sieve t2)
prime_sieve until = sieve [n | n <- [2..until]]

--euler_10 = sum (primes_until 1999999)
euler_10 = sum (prime_sieve 1999999)
--euler_10 = sum (prime_sieve 19999)

enumerate_list [] _ = []
enumerate_list (e:rest) n = (n,e):(enumerate_list rest (n + 1))

euler_7 = enumerate_list (take 10001 primes) 1

main = do
  putStrLn $ show $ euler_7
