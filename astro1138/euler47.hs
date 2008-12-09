import Data.List (nub, intersect)
import Debug.Trace (trace)

primes :: Int -> Int -> [Int]
primes n d | d <= n = if n `rem` d == 0
                      then d:(primes (n `div` d) d)
                      else primes n (d + 1)
primes _ _ = []

--primefactors n = trace ("primefactors " ++ (show n)) $ primes n 2
primefactors n = primes n 2

has_dups [l] = False
has_dups (a:b:l) | a `intersect` b /= [] = True
                 | otherwise = has_dups (b:l)

euler47' a numbers | is_desired = numbers'
                   | otherwise = euler47' a rest'
    where numbers' = take a numbers
          factors = map (nub . primefactors) numbers'
          is_desired = all (((==) a) . length) factors && (not $ has_dups factors)
          (_:rest') = numbers

euler47 = euler47' 4 [2..]
