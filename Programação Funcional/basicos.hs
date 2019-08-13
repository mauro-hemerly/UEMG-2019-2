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

quadradoMaisUm :: Float -> Float
quadradoMaisUm x = x * x + 1




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
        | True = "Conceito E"


maior2 :: Int -> Int -> Int
maior2 x y = if x > y then x else y

maior2' :: Int -> Int -> Int
maior2' x y
    | x > y = x
    | senao = y

senao :: Bool
senao = True


maior3 :: Int -> Int -> Int -> Int
maior3 x y z
    | x > y && x > z = x
    | y > z = y
    | otherwise = z


-- Casamento de Padrão
match 10 = print 10
match 'P' = print 'P'
match True = print True

match param = print param

match _ = print "Ok..."




fatorial :: Int -> Int
fatorial n = if n > 0 then n * fatorial (n-1) else 1

fatorial' :: Int -> Int
fatorial' 0 = 1
fatorial' n 
     | n > 0 = n * fatorial' (n-1)
