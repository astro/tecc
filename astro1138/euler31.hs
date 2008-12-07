coins = [(a,b,c,d,e,f,g,h) |
                             a <- [0..200],
                             b <- [0..(100 - a `div` 2)],
                             a + b * 2 <= 200,
                             c <- [0..(40 - (a `div` 5) - (b `div` 2))],
                             d <- [0..20],
                             a + b * 2 + c * 2 + d * 10 <= 200,
                             e <- [0..10],
                             f <- [0..4],
                             g <- [0..2],
                             h <- [0..1],
                             a + b * 2 + c * 2 +
                               d * 10 + e * 20 + f * 50 +
                                 g * 100 + h * 200 == 200]

euler_31 = length coins

main = do
  putStrLn $ show $ euler_31
