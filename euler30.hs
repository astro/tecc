import Data.Char (ord)

hoch n 0 = 1
hoch n m = n * (n `hoch` (m - 1))

digits n = map (\c -> (ord c) - (ord '0')) $ show n

euler30' n = n == n_sum
    where n_sum = sum n_sqs
          n_sqs = map (\m -> m `hoch` 5) d
          d = digits n

euler30 = (numbers, sum numbers)
    where numbers = filter euler30' [2..(5 * (9 `hoch` 5))]
