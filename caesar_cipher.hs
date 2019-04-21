import Data.Char

alphabet :: [Char]
alphabet = ['a'..'z']

let2int :: Char -> Int
let2int c = ord c - ord (head alphabet)

int2let :: Int -> Char
int2let n = chr (ord (head alphabet) + n)

shift :: Int -> Char -> Char
shift n c
  | isLower c = int2let ((let2int c + n) `mod` length alphabet)
  | not $ toLower c `elem` alphabet = c
  | otherwise = toUpper (shift n (toLower c))

encode :: Int -> String -> String 
encode n xs = [shift n x | x <- xs]

decode :: Int -> String -> String
decode n xs = [shift (-n) x | x <- xs]
