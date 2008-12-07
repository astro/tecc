import List

is_palindrome :: Int -> Bool
is_palindrome n = s == (reverse s)
    where s = show n

all_3_palindromes = sort [a * b | a <- [100..999], b <- [100..999],
                          is_palindrome $ a * b]