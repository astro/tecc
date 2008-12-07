hoch n 1 = n
hoch n e = n * hoch n e'
    where e' = e - 1

sum_digits 0 = 0
sum_digits n = n1 + n2
    where n1 = sum_digits n'
          n2 = n `rem` 10
          n' = n `div` 10

euler16 = sum_digits n2h1000
    where n2h1000 = hoch 2 1000
