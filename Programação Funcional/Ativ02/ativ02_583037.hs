nome = "Ana Beatriz Pereira Capistrano"


matricula = "583037"


-- atividade 01


-- (1) criar função que dado um número inteiro gere sua fatoração na forma de uma lista de duplas. Por exemplo,


-- 72 = 2^3  .  3^2


-- de onde a lista deve ser,


-- [(2,3),  , (3,2)]


-- construa função com o cabeçalho,
 

fprimos :: Int -> [(Int,Int)]
fprimos n = fatorar n 2

fatorar :: Int -> Int -> [(Int,Int)]
fatorar x y = if y > x then [] else if ehPrimo y && mod x y == 0 then (y, count x y) : fatorar x (y+1) else fatorar x (y+1)

count :: Int -> Int -> Int
count n d = if mod n d /= 0 then 0 else  1 + count (div n d) d

dividir :: Int -> Int -> Int
dividir n d = if mod n d /= 0 then n else dividir (div n d) d
ehPrimo :: Int -> Bool
ehPrimo x = if x < 2 then False else ehPrimo' x 2

ehPrimo' :: Int -> Int -> Bool
ehPrimo' x n = if n*n > x then True else if mod x n == 0 then False else ehPrimo' x (n+1)

-- (2) Seja uma strings s da qual se deseja construir a lista das frequências dos CARACTERES. Cada frequência é uma dupla formada pelo caractere e o total de vezes que ele acontece. Por exemplo,


-- s = "aaabb222"


-- deve gerar a lista,


-- [('a',3), ('b',2), ('2',3)] 


-- construa função com o cabeçalho,
--usar filter

freq :: String -> [(Char,Int)]
freq s = if (stringVazia s == True) then [] else (primeiro, countFreq primeiro s) : freq (limpar primeiro s) where primeiro = head s

countFreq :: Char -> String -> Int 
countFreq c str = length(filter (== c) str)

limpar :: Char -> String -> String
limpar c str = filter (/= c) str

stringVazia :: String -> Bool
stringVazia x = length x == 0

