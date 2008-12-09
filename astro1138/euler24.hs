import Data.List (delete)

pick' :: [Int] -> [Int] -> [(Int, [Int])]
pick' _ [] = []
pick' l (e:l') = (e, delete e l):(pick' l l')
pick l = pick' l l

permute :: [Int] -> [[Int]]
permute [] = [[]]
permute l = [c:r' | (c, r) <- pick l,
                    r' <- (permute r)]

euler24 = (permute [0..9]) !! 999999
