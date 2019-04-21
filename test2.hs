safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs

safetail' :: [a] -> [a]
safetail' xs  | null xs = [] 
              | otherwise = tail xs

safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' (_:xs) = xs

or' :: Bool -> Bool -> Bool
or' True _ = True
or' _ True = True
or' _ _ = False

or'' :: Bool -> Bool -> Bool
or'' False False = False
or'' _ _ = True

or''' :: Bool -> Bool -> Bool
or''' False a = a
or''' True a = True

and' True True = True
and' _ _ = False

and'' :: Bool -> Bool -> Bool
and'' a b = if a == True && b == True then True else False

and''' :: Bool -> Bool -> Bool
and''' True b = b 
and''' False _ = False

and'''' :: Bool -> Bool -> Bool
and'''' a b = if a == True then b else False

