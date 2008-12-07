import List (sortBy)

s1 n | n `rem` 2 == 0 = n `div` 2
     | otherwise = 3 * n + 1

set_nth v (_:list) 1 = v:list
set_nth v (e:list) n = e:list'
    where list' = set_nth v list n'
          n' = n - 1
set_nth v [] 1 = [v]
set_nth v [] n = 0:list'
    where list' = set_nth v [] n'
          n' = n - 1

resolve_lengths :: [Int] -> Int -> [Int]
resolve_lengths results 1 = results
resolve_lengths results n = if results !! i == 0
                            then set_nth l' results' i
                            else results
    where i = n - 1
          l' = l
          l = results' !! i
          results' = resolve_lengths results n'
          n' = s1 n

euler14' :: [Int] -> Int -> [Int]
euler14' results n = if n < 1000000
                     then euler14' results' n'
                     else results
    where results' = resolve_lengths results n
          n' = n + 1


s' 1 = []
s' n = n:rest
    where rest = s' n'
          n' = if n `rem` 2 == 0
               then n `div` 2
               else 3 * n + 1

s n = s' n

s_until limit = [(n,s n) | n <- [1..limit]]

euler14 = head $ sortBy (\(_,l1) (_,l2) ->
                             compare (length l1) (length l2)) $
          s_until 999999

main = do
  putStrLn $ show $ euler14
