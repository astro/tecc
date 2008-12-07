fac 1 = 1
fac n = n * fac n'
    where n' = n - 1

sum_digits 0 = 0
sum_digits n = n1 + n2
    where n1 = sum_digits n'
          n2 = n `rem` 10
          n' = n `div` 10

euler20 = sum_digits f100
    where f100 = fac 100
