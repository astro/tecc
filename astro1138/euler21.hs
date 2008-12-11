d n = sum divisors
    where divisors = filter (\m -> n `rem` m == 0)
                     [1..(n `div` 2)]

is_amicable n = n /= (d n) && n == (d $ d n)

euler21' = filter is_amicable [1..9999]

euler21 = sum euler21'

