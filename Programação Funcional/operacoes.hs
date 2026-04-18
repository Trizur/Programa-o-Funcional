--dobro
dobrar :: Int -> Int
dobrar x = 2 * x
--triplo
triplo :: Int -> Int
triplo x = 3 * x
--soma
soma :: Int -> Int -> Int
soma x y = x + y
--diferença do quadrado
difQuadrado :: Int -> Int -> Int
difQuadrado x y = (x * x) - (y * y)
--par ou impar
fnc1 :: Int -> String
fnc1 x = if mod x 2 == 0 then "par" else "impar"
--fatorial
fat1 :: Integer -> Integer
fat1 n = if n < 2 then 1 else n * fat1 (n - 1)
--potencia
pow :: Integer -> Integer -> Integer
pow x n = if n == 0 then 1 else x * pow x (n - 1)
-- doubleUs
doubleUs :: Int -> Int -> Int
doubleUs x y = dobrar x + dobrar y
--dobre o menor
dobrarMenor :: Int -> Int -> Int
dobrarMenor x y = if x < y then (dobrar x) else (dobrar y) 
--dobre o maior
dobrarMaior :: Int -> Int -> Int
dobrarMaior x y = if x > y then (dobrar x) else (dobrar y) 