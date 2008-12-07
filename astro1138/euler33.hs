import Data.Char (ord, chr)

fractions = [(a,b) | a <- [10..99], b <- [10..99]]

b_to_s b = map chr b
digits n = [ord c - ord '0' | c <- show n]

cancel_digit d n = (read $ b_to_s n'')::Int
    where n'' = filter (/= d) n'
          n' = show n

cancelations a b = [(cancel_digit digit a, cancel_digit digit b) | digit <- show a]

eq (a, b) (a', b') = a * b' == a' * b

is_cancelable (a, b) = any (\(a',b') ->
                                (a, b) `eq` (a', b')) $
                       cancelations a b

euler_33 = filter is_cancelable fractions
