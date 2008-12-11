import Data.List (sort, nub)

is_pandigital lists = total_length == 9 && sort list == ['1'..'9']
    where total_length = foldl (+) 0 $ map length lists
          list = concat lists

euler32' = [c
            | (a, b, c) <- [(a, b, a * b)
                            | a <- [1..10000], b <- [1..1000]],
            is_pandigital $ map show [a, b, c]]

euler32 = sum $ nub euler32'