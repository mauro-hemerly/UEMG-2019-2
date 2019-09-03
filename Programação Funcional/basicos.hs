main :: IO ()
main = print (maior2' 10 20)

-- Comnetário

{--
 Comentários em bloco
--}

idade :: Int -- Um valor inteiro constante
idade = 19


maiorDeIdade :: Bool -- Usa a definição de idade
maiorDeIdade = idade /= 18

quadrado :: Int -> Int -- função que eleva um número ao quadrado
quadrado x = x * x

quadradoMaisUm :: Int -> Int
quadradoMaisUm x = quadrado x + 1




-- Estrutura de controle
-- if <condition> then <true-value> else <false-value>
conceito :: Int -> String
conceito x = if x > 90 then "Conceito A"
             else if 80 < x && x < 90 then "Conceito B"
             else if 70 < x && x < 80 then "Conceito C"
             else if 60 < x && x < 70 then "Conceito D"
             else "Conceito E"

-- Utilizando guardas
conceito' :: Int -> String
conceito' x 
        | x > 90 = "Conceito A"
        | 80 < x && x < 90 = "Conceito B"
        | 70 < x && x < 80 = "Conceito C"
        | 60 < x && x < 70 = "Conceito D"
        | otherwise = "Conceito E"

--type String = [Char]
-- otherwise :: Bool
-- otherwise = True

senao :: Bool
senao = True

type Nome = String
type Endereco = String
type Idade = Int
type Pessoa = (Nome, Endereco, Idade)

pessoa :: Pessoa
pessoa = ("Mauro", "Rua", 22)


maior2 :: Int -> Int -> Int
maior2 x y = if x > y then x else y

maior2' :: Int -> Int -> Int
maior2' x y
    | x > y = x
    | otherwise = y



maior3 :: Int -> Int -> Int -> Int
maior3 x y z
    | x > y && x > z = x
    | y > z = y
    | otherwise = z


-- Casamento de Padrão
match 10 = 100
match 20 = 200
match x = x + 1000

-- Casamento de Padrão com Tuplas
match2 (18,True) = "Ok..."
match2 (97,True) = "Ok..."
--match2 x y = x + y
--match2 (nome,sexo,_) = "Ok..."
match2 (x,y) = "Ok..."


-- Converte digito decimal para sua extensão
convDigitoText :: Int -> String
convDigitoText num = 
    case num of
        0 -> "zero"
        1 -> "um"
        2 -> "dois"
        3 -> "três"
        4 -> "quatro"
        5 -> "cinco"
        6 -> "seis"
        7 -> "sete"
        8 -> "oito"
        9 -> "nove"

fst' :: (a,b) -> a
fst' (x,_) = x

snd' :: (b,a) -> a
snd' (_,y) = y


second' :: [a]-> a
second' (x:y:xs) = y

maiorLista [x] = x
maiorLista (x:xs)
    | x > maiorLista xs = x
    | otherwise = maiorLista xs


somaTupla :: (Int,Int)->(Int,Int)-> (Int,Int)
somaTupla (x,y) (z,w) = (x+z,y+w)

-- Sem casamento de padrão
match' :: Int -> Int
match' x 
      | x == 10 = 100
      | x == 20 = 200
      | senao = x + 1000


(|||) :: Bool->Bool->Bool
(|||)  True _ = True
(|||)  _ True = True
(|||)  _ _ = False


fatorial :: Int -> Int
fatorial n = if n > 0 then n * fatorial (n-1) else 1

fatorial' :: Int -> Int
fatorial' 0 = 1
fatorial' n 
     | n > 0 = n * fatorial' (n-1)
