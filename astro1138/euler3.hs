import System

primes :: Int -> Int -> [Int]
primes n d | d <= n = if n `rem` d == 0
                      then d:(primes (n `div` d) d)
                      else primes n (d + 1)
primes _ _ = []

primefactors n = primes n 2

main = do
  [arg] <- getArgs
  putStrLn $ show $ primefactors ((read arg)::Int)


