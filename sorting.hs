qsort :: Ord t => [t] -> [t]
qsort [] = []
qsort (x:xs) = reverse (qsort smaller ++ [x] ++ qsort larger)
    where smaller = [a | a <- xs, a <= x]
          larger =  [b | b <- xs, b > x]

