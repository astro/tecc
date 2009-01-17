import Utils

data NumberType = Deficient | Perfect | Abundant
                  deriving (Show, Eq)

max_sum' _ [] sum = sum
max_sum' max (e:l) sum | sum' > max = sum'
                       | otherwise = max_sum' max l sum'
                       where sum' = sum + e

max_sum max l = max_sum' max l 0

number_type n | n > d = Deficient
              | n == d = Perfect
              | n < d = Abundant
              where d = max_sum (n + 1) $ divisors n

sum_of_2_abundant_numbers n = any (\(a, b) ->
                                       number_type a == Abundant &&
                                       number_type b == Abundant)
                              [(a, b)
                               | a <- [1..n], b <- [1..n],
                               a + b == n]

euler_23' n sum
    | n > 28123 = []
    | sum_of_2_abundant_numbers n = euler_23' n' sum
    | otherwise = (n, sum'):(euler_23' n' sum')
    where n' = n + 1
          sum' = sum + n

euler_23 = euler_23' 1 0

main = do
  putStrLn $ show euler_23
