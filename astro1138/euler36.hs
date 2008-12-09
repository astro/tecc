import Data.Char (ord)


digits10 n = map (\c -> (ord c) - (ord '0')) $ show n
digits2' 0 = [0]
digits2' n = i:(digits2' r)
    where i = n `rem` 2
          r = n `div` 2

digits2 n = dropWhile ((==) 0) $ reverse $ digits2' n

is_palindrome l = l == (reverse l)

euler36' n = p10 && p2
    where p10 = is_palindrome $ digits10 n
          p2 = is_palindrome $ digits2 n

euler36 = (l, sum l)
    where l = [n | n <- [1..999999], euler36' n]
