-- Ana Beatriz Pereira Capistrano 583037

-- Universidade Federal do Ceará
-- Campus de Quixadá
-- Programaçãop Funcional
-- Atividade 01
-- Professor Ricardo Reis


-- identificação
nome = "Ana Beatriz Pereira Capistrano"
matricula = "583037"


-- função 01: transformar inteiro na lista de seus dígitos 


digs:: Int -> [Int]
digs n = if n < 10 then [n] else digs (div n 10) ++ [mod n 10]

-- exemplo
-- digs 123
-- [1,2,3]


-- função 02: somar os módulos das entradas de uma lista de inteiros


sabs :: [Int] -> Int
sabs ls = if length ls == 0 then 0 else abs (head ls) + sabs (tail ls)

-- função 3: retornar o maior valor absoluto  de uma lista de inteiros. de
maximum' :: [Int] -> Int 
maximum' ls = if length ls == 1 then abs (head ls) else max (abs(head ls))(maximum'(tail ls))


-- função 4: retorna a frequência de um caractere numa string
freq:: String -> Char  -> Int 
freq s ch = if length s == 0 then 0 else if head s == ch then 1 + freq(tail s) ch else freq(tail s) ch

