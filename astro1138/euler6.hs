import List

sum_square :: Int -> Int
sum_square n = fromInteger $ s ** 2
    where s = toInteger (sum ns)
          ns = [1..n]

squares_sum :: Int -> Int
squares_sum n = sum squares
    where squares = [toInteger $ x ** 2 | x <- ns]
          ns = [1..n]

diff_upto :: Int -> Integer
diff_upto n = toInteger $ abs $ (sum_square n) - (squares_sum n)