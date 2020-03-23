quick_sort :: (Ord d) => [a] -> [a]
quick_sort [] = []
quick_sort [x:xs] = quick_sort ls ++ [x] ++ quick_sort rs
  where
    ls = [l | l <- xs, l <= x]
    rs = [r | r <- xs, r > x]

quick_sort [3, 5, 4, 7, 1]