
inverso :: [Int] -> [Int]
inverso ls = 
 if length ls < 2 
   then ls 
   else inverso (tail ls) ++ [head ls]

   --Uso da função "zip" 
   --Ex: zip [1,2,3] [4,5,6] ==> [(1,4),(2,5),(3,6)]
   