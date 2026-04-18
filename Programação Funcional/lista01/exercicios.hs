--01  menorDeDois
--INPUT: Dois números, x e y
--OUTPUT: menor valor entre x e y
menor :: Int->Int->Int
menor x y = if x < y then x else y
--02  menorDeTres
-- INPUT: Três números, x, y e z
-- OUTPUT: menor valor entre x, y e z
menorDeTres :: Int->Int->Int->Int
menorDeTres x y z = menor (menor x y) z
--03  fatorial
-- INPUT: Um natural n
-- OUTPUT: O fatorial de n
fat :: Integer -> Integer
fat n = if n < 2 then 1 else n * fat (n - 1)
-- 04fibonacci
-- INPUT: Inteiro positivo n
-- OUTPUT: n-ésimo termo da sequência de Fibonacci
-- (iniciando em com 0 e 1)
fib :: Integer -> Integer
fib n = if n < 2 then n else fib (n - 1) + fib (n - 2)
--05  elemento
-- INPUT: Lista u e um natural n
-- OUTPUT: n-ésimo termo de u
-- EX(S):
-- elemento 2 [2,7,3,9] ==> 3
elemento :: Int -> [Int] -> Int
elemento n u = if n == 0 then head u else elemento (n - 1) (tail u)





