import List
import System

divisable_upto n = head [x | x <- [1..],
                         all (\m -> x `rem` m == 0) [1..(n - 1)]]

main = do
  [arg] <- getArgs
  putStrLn $ show $ divisable_upto ((read arg)::Int)
