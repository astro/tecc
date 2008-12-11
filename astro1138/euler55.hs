reverse_number n = r::Integer
    where r = read $ reverse $ show n

chain n = m:(chain m)
    where m = n + (reverse_number n)

is_palindromic n = any test $ take 50 $ chain n
    where test m = m == (reverse_number m)


euler55' = filter (not . is_palindromic) [1..9999]

euler55 = length euler55'
