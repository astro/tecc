import Data.List (mapAccumL, elemIndex)

-- 21 22 23 24 25
-- 20  7  8  9 10
-- 19  6  1  2 11
-- 18  5  4  3 12
-- 17 16 15 14 13

-- 1 1 2 2 3 3 4 4 5 5

data Vector = Vec2 Int Int
              deriving (Show, Eq)

vadd (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (x1 + x2) (y1 + y2)

rotate_clockwise (Vec2 x y) = Vec2 (-y) x

spiral' pos dir lens = positions ++ rest
    where (nextpos, positions) = mapAccumL (\p _ -> (p `vadd` dir, p)) pos [1..len1]
          rest = spiral' nextpos dir' lens'
          dir' = rotate_clockwise dir
          [len1, len2] = lens
          lens' = if len1 == len2
                  then [len2, len2 + 1]
                  else [len2, len2]

spiral = spiral' (Vec2 0 0) (Vec2 1 0) [1, 1]

spiral_value_at pos = i + 1
    where Just i = elemIndex pos spiral

spiral_x_sum size = sums + o
    where m = size `div` 2
          o = spiral_value_at $ Vec2 0 0
          sums = sum $
                 [spiral_value_at $ Vec2 (n * xs) (n * ys)
                      | n <- [1..m],
                        xs <- [-1, 1],
                        ys <- [-1, 1]]

