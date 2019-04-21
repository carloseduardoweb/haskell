partition :: Ord a => [a] -> a -> ([a],[a])
partition [] _ = ([],[])
partition (x:xs) p | x < p     = (x:a,b)
                   | otherwise = (a,x:b)
   where
      (a,b) = partition xs p
