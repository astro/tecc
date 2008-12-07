import List (sortBy)

number_digits number = map (\c -> (read [c])::Int)
                       $ show number

sieve :: [Int] -> [Int]
sieve [] = []
sieve (n:t) = let t2 = [m | m <- t, m `rem` n /= 0]
              in
                n:(sieve t2)
prime_sieve until = sieve [n | n <- [2..until]]

enumerate_list [] _ = []
enumerate_list (e:rest) n = (n,e):(enumerate_list rest (n + 1))


--euler35 = enumerate_list (filter (\prime -> ) (prime_sieve 1000000)) 1