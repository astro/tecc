import Data.Char

gen_number' n = s ++ s'
    where s = show n
          s' = gen_number' $ n + 1

gen_number = gen_number' 1

digit ns n = (ord d) - (ord '0')
    where d = ns !! (n - 1)

hoch n 0 = 1
hoch n m = n * (n `hoch` (m - 1))

euler40 = foldl (*) 1 [digit gen_number $ 10 `hoch` n | n <- [0..6]]

main = do
  putStrLn $ show euler40