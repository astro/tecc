module Utils (primes, primefactors, divisors) where

import Data.List (nub, subsequences, foldl')

is_prime 1 = False
is_prime 2 = True
is_prime x = not (any (\n -> x `rem` n == 0) [2..(x `div` 2)])

primes = [x | x <- [1..], is_prime x]


primefactors' :: Int -> Int -> [Int]
primefactors' n d | d <= n = if n `rem` d == 0
                             then d:(primefactors' (n `div` d) d)
                             else primefactors' n (d + 1)
primefactors' _ _ = []

primefactors n = primefactors' n 2


--divisors x = filter (\n -> x `rem` n == 0)
--             [1..(x `div` 2)]

divisors x = d'
    where d' = filter (/= x) d
          d = map (foldl' (*) 1) $ nub $ subsequences p
          p = primefactors x
