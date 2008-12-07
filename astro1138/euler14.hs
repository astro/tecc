seq_succ 1 = 1
seq_succ n | n `rem` 2 == 0 = n `div` 2
           | otherwise = 3 * n + 1

euler14' sequences [] = []
euler14' sequences (n:rest) = (n,n_length):(euler14' sequences' rest)
    where l m | m < n = (sequences !! (n - m - 1))
              | otherwise = (l (seq_succ m)) + 1
          n_length = l n
          sequences' = n_length:sequences

pickMax l = pickMax' l 0
pickMax' [] _ = []
pickMax' (e:l) max | max < elen = e:(pickMax' l elen)
                   | otherwise = pickMax' l max
                   where (_,elen) = e

euler14 until = pickMax starts_lengths
    where starts_lengths = euler14' [1, 0] [2..until]

main = do
    putStrLn $ show $ euler14 999999
