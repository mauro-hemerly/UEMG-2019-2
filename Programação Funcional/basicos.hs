-- Mauro Hemerly (Hämmerli) Gazzani
-- Disciplina de Programação Funcional
-- Curso de Engenharia de Computação
-- UEMG - Unidade de Ituiutaba

import Data.Char
import Prelude

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

-- type Nome = String
-- type Endereco = String
-- type Idade = Int
-- type Pessoa = (Nome, Endereco, Idade)

-- pessoa :: Pessoa
-- pessoa = ("Mauro", "Rua", 22)


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



maiuscula::[Char]->[Char]
maiuscula [] = []
maiuscula (a:as) = toUpper a: maiuscula as

soma :: [Int] -> Int
soma [] = 0
soma (x:xs) = 1 + soma xs

conta :: [a] -> Int
conta [] = 0
conta (x:xs) = 1 + conta xs

media :: [Int] -> Float
media xs = fromIntegral (soma xs) / fromIntegral (conta xs)





-- Função de Ordem Superior (High-order Function)

func :: (Int -> Int) -> Int -> Int
func f x = 100 + f x


dobro :: Int -> Int
dobro x = 2*x

triplo :: Int -> Int
triplo x = 3*x


dobraLista :: [Int] -> [Int]
dobraLista [] = []
dobraLista (x:xs) = 2*x : dobraLista xs




-- Função map' : aplica a função f a cada elemento da lista
map' :: (a -> a) -> [a] -> [a]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

comparaChar :: Char -> Char -> Bool
comparaChar ch1 ch2 = ch1 == ch2

extraiPrimeiro :: String -> Char
extraiPrimeiro (x:xs) = x
      
excluiPrimeiro :: String -> String
excluiPrimeiro (x:xs) = xs

-- Função filter' de Mais Alta Ordem
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
        | f x = x : filter' f xs
        | otherwise = filter' f xs

listUp' :: Int->[Int]
listUp' n = listUp'' 1 n

listUp'' :: Int->Int->[Int]
listUp'' c n = if (c == n) then [n] else c : listUp'' (c+1) n

--Questão 4 - Lista 3
f1 f x 
    | (f x) > x = 100
    | otherwise = 20

-- Definição de Novos Tipos de Dados

data Cor = Vermelho | Verde | Azul

data Endereco = Rua String Int Complemento
data Complemento = Casa | Apartamento

-- Definição de Classe de Tipos
class Compara a where
  eIgual :: a -> a -> Bool
  eIgual x y = not (naoEIgual x  y)

  naoEIgual :: a -> a -> Bool
  naoEIgual x y = not (eIgual x y)

  (=:=) :: a -> a -> Bool
  x =:= y = not (x /=:= y)

  (/=:=) :: a -> a -> Bool
  x /=:= y = not (x =:= y)


instance Compara Cor where
  eIgual Vermelho Vermelho = True
  eIgual Verde Verde = True
  eIgual Azul Azul = True
  eIgual _ _ = False
  Vermelho =:= Vermelho = True
  Verde =:= Verde = True
  Azul =:= Azul = True
  _ =:= _ = False

instance Compara Endereco where
  Rua x y z =:= Rua a b c = x == a && y == b && z =:= c

instance Compara Complemento where
  Casa =:= Casa = True
  Apartamento =:= Apartamento = True
  _ =:= _ = False

instance (Compara a) => Compara [a] where
  eIgual [] [] = True
  eIgual (x:xs) (y:ys) = eIgual x y && eIgual xs ys
  eIgual _ _ = False

  (=:=) [] [] = True
  (=:=) (x:xs) (y:ys) = (=:=) x y && (=:=) xs ys
  (=:=) _ _ = False



-------------------------------------

iguala :: NomeCompleto -> NomeCompleto -> Bool
iguala (Nome x) (Nome y) = x == y


data NomeCompleto = Nome String deriving Eq
--instance Eq NomeCompleto where
--  (==) = iguala 

data Coisa a = Nada | UmaCoisa a | DuasCoisas a a deriving Show


data Booleano = Verdadeiro | Falso 
                deriving(Show, Eq, Ord)     

type ISBN = String
type Titulo = String
type Autores = [String]

data LivroInfo = Livro ISBN Titulo Autores 
                  deriving(Show)  

meuLivro = Livro "1234" "Haskell 24 horas" ["Mauro Hemerly","Eduardo Costa"]

