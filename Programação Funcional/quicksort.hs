
quicksort :: Ord a => [a] -> [a]
quicksort ls = if length ls < 2 then ls
 else x ++ [m] ++ y
 where
    --m vai ser do índice 0
 m = ls !! 0
 --Uma lista de x tal que x é retirado de ls e x é menor que m
 x = quicksort[x | x <- ls, x < m] 
  --Uma lista de x tal que x é retirado de ls e x é maior que m
 y = quicksort[x | x <- ls, x > m]