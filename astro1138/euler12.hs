triangle_number n = n * (n + 1) `div` 2

divisors n = n:d
    where nh = n `div` 2
          d =  filter (\m -> n `rem` m == 0) [1..nh]

tndc n = length $ divisors t
    where t = triangle_number n

euler12 start = if d > 500
                then [current]
                else current:(euler12 start')
    where start' = start + 1
          d = tndc start
          current = (start, d)

euler12_fuzzy' start = if d > 500
                       then current:(euler12_fuzzy' start')
                       else euler12_fuzzy' start'
    where start' = start - 1
          d = tndc start
          current = (start, d)
euler12_fuzzy start = if d > 500
                      then current:(euler12_fuzzy' start'')
                      else current:(euler12_fuzzy start')
    where start' = start * 10
          start'' = start - 1
          d = tndc start
          current = (start, d)

