import List (sort)

hoch n 1 = n
hoch n e = n * hoch n e'
    where e' = e - 1

gen_seq a1 a2 b1 b2 = [a `hoch` b | a <- [a1..a2], b <- [b1..b2]]

rm_dups [] = []
rm_dups (e1:e2:rest) | e1 == e2 = rm_dups (e2:rest)
rm_dups (e:rest) = e:(rm_dups rest)

uniq_seq a1 a2 b1 b2 = without_dups
    where without_dups = rm_dups sorted
          sorted = sort s
          s = gen_seq a1 a2 b1 b2

euler29 = length $ uniq_seq 2 100 2 100
