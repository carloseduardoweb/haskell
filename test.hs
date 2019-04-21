double x = 2 * x

quadruple x = double (double x)

factorial n = product [1..n]

average ns = div (sum ns) (length ns)

a = b + c
  where 
    b = 1
    c = 2

n = a `div` length xs
  where
    a = 10
    xs = [1,2,3,4,5]

last' xs = head (reverse xs)

last'' xs = xs !! ((-) (length xs) 1)

head' (x:_) = x

tail' (_:xs) = xs

init' xs = reverse(tail(reverse xs))

sum' :: Num t => [t] -> t
sum' xs = sum xs

palindrome xs = reverse xs == xs

twice :: (t -> t) -> t -> t
twice f x = f (f x)

twice' :: (t -> t -> t) -> t -> t -> t
twice' f x y = f (f x y) y

abs' :: (Num t, Ord t) => t -> t
abs' a = if a >= 0 then a else (-a)

signum' a =  if a > 0 then '+' else
                  if a == 0 then ' ' else '-'

abs'' a | a > 0 = a
        | a < 0 = (-a)
        | otherwise = 0

and :: Bool -> Bool -> Bool
and True a = a
and False _ = False

