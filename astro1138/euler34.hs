import Data.Char (ord)

digits n = map (\c ->
                    (ord c) - (ord '0')) $
           show n

fac n | n < 0 = 0
fac 1 = 1
fac n = n * fac n'
    where n' = n - 1

euler_34 = [n | n <- [1..400000000],
            n >= 10,
            n == (sum $ map fac $ digits n)]

main = do
  putStrLn $ show $ euler_34
