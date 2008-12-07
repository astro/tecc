euler_9 = [(a,b,c) | a <- [1..1000], b <- [1..1000], c <- [1..1000],
           a + b + c == 1000,
           a < b && b < c,
           a * a + b * b == c * c]

main = do
  putStrLn $ show euler_9
