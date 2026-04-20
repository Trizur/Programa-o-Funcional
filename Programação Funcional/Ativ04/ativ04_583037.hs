atividade = "04"
nome = "Ana Beatriz Pereira Capistrano"
matricula = "583037"


--  1


replace :: [Char] -> [Char] -> [Char] -> [Char]
replace [] _ _ = []
replace (x:xs) from to
    | match from (x:xs) = to ++ replace (skip from (x:xs)) from to
    | otherwise = x : replace xs from to

match :: [Char] -> [Char] -> Bool
match [] _ = True
match _ [] = False
match (x:xs) (y:ys)
    | x == y = match xs ys
    | otherwise = False

skip :: [Char] -> [Char] -> [Char]
skip [] ys = ys
skip (_:xs) (_:ys) = skip xs ys
skip _ [] = []

-- 2


lsSplit :: [Int] -> ([Int], Int, [Int])
lsSplit [] = ([], 0, [])
lsSplit ls = splitAux [] ls (maximum ls)

splitAux acc [] m = (acc, m, [])
splitAux acc (x:xs) m
    | x == m = (acc, m, xs)
    | otherwise = splitAux (acc ++ [x]) xs m

-- 3


selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort ls = maior : selectionSort resto
    where
        maior = maximum ls
        resto = delete' maior ls

delete' :: Eq a => a -> [a] -> [a]
delete' _ [] = []
delete' y (x:xs)
    | y == x = xs
    | otherwise = x : delete' y xs

