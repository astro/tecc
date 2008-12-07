data Rational = R Integer Integer
                deriving (Show)

radd a (R n2 d2) = (R (a * d2 + n2) d2)
rdiv a (R n2 d2) = R (a * d2) n2

root2' :: Int -> Main.Rational
root2' 1 = R 2 1
root2' n = 2 `radd` (1 `rdiv` (root2' $ n - 1))
root2 :: Int -> Main.Rational
root2 n = 1 `radd` (1 `rdiv` (root2' n))

gen_series iterations = [root2 i | i <- iterations]

num_gt_denom r = nl > dl
    where (R n d) = r
          l x = length $ show $ abs x
          nl = l n
          dl = l d

euler_57 = length $ filter num_gt_denom series
    where series = gen_series [1..1000]
