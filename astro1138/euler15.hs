ways x y | x < 1 || y < 1 = 1
         | otherwise = right + down
    where right = if x > 0
                  then ways (x - 1) y
                  else 0
          down = if y > 0
                 then ways x (y - 1)
                 else 0


enumerate' [] _ = []
enumerate' (e:l) n = (n,e):l'
    where l' = enumerate' l n'
          n' = n + 1
enumerate l = enumerate' l 1

euler_15 = ways 20 20

main = do
  putStrLn $ show $ euler_15


build_grid w h grid
    | (length top_line) == w && (length grid) == h = grid
    | (length top_line) == w = build_grid w h ([]:grid)
    | otherwise = build_grid w h ((ways:top_line):grid1)
    where (top_line:grid1) = grid
          ways_right = if (length top_line) > 0
                       then top_line !! 0
                       else 0
          ways_down = if (length grid1) > 0
                      then let below_line = grid1 !! 0
                               x = w - (length top_line) - 1
                           in below_line !! x
                      else 0
          ways = ways_right + ways_down

d = build_grid 21 21 [[1]]
