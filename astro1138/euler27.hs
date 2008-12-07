import Data.List (sortBy)

is_prime x | x < 1 = False
is_prime 1 = False
is_prime 2 = True
is_prime x = not (any (\n -> x `rem` n == 0) [2..(x `div` 2)])

gen_primes' :: Int -> Int -> Int -> Int
gen_primes' a b n = if is_prime r
                    then gen_primes' a b n'
                    else n
    where r = n * n + a * n + b
          n' = n + 1
gen_primes a b = gen_primes' a b 0

euler_27' = [(as * a, bs * b, gen_primes (as * a) (bs * b)) |
                as <- [-1, 1],
                a <- [0..1000],
                bs <- [-1, 1],
                b <- [0..1000],
                (gen_primes (as * a) (bs * b)) > 0]

euler_27 = take 10 $ reverse sorted
    where sorted = sortBy (\(_,_,p1) (_,_,p2) ->
                               compare p1 p2)
                   euler_27'
