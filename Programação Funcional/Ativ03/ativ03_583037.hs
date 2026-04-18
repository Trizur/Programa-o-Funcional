nome = "Ana Beatriz Pereira Capistrano"
matricula = "583037"




-- Fundir dois vetores ordenados num vetor ordenado maior.
-- use casamento de padrões.
-- não use meios externos de ordenação.
-- use recursão.


merge :: (Ord a) => [a] -> [a] ->[a]
merge u v = if null u then v 
            else if null v then u
            else case (u, v) of
                (x:xs, y:ys) ->
                    if x <= y
                    then x : merge xs (y:ys)
                    else y : merge (x:xs) ys


-- implemente mergesort para 
-- ordenação do vetor u.
--   Use a função anterior.


mergesort ::  (Ord a) => [a] -> [a]
mergesort u = if length u <= 1 then u
              else merge (mergesort esq) (mergesort dir)
              where 
                meio = div (length u) 2
                esq = take meio u
                dir = drop meio u




-- usando fold implementar função que retorne 
-- a série de Fibonacci com n elementos.


fibo'list :: Int -> [Int]  
fibo'list m = if m <= 0 then []
              else if m == 1 then [0]
              else take m (foldl passo [0,1] [2..m])
              where 
                passo acc _ = 
                    acc ++ [last acc + last (init acc)]

