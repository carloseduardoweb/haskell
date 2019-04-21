import Prelude hiding ((||))

halve :: [a] -> ([a], [a])
halve xs = (ls, rs)
  where ls = take half xs
        rs = drop half xs
        half = if even len then div len 2 else (+1) (div len 2)
        len = length xs

halve' :: [a] -> ([a], [a])
halve' xs = splitAt (div (length xs) 2) xs

(||) :: Bool -> Bool -> Bool
False || False = False
_ || _ = True

find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k', v) <- t, k' == k]


