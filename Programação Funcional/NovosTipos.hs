-- Mauro Hemerly (Hämmerli) Gazzani
-- Disciplina de Programação Funcional
-- Curso de Engenharia de Computação
-- UEMG - Unidade de Ituiutaba

-- Definição de Novos Tipos de Dados

data Cor = Vermelho | Verde | Azul deriving Eq

data Endereco = Rua String Int Complemento
data Complemento = Casa | Apartamento


-- Definição de Classe de Tipos
class Compara a where
  eIgual :: a -> a -> Bool
  eIgual x y = not (naoEIgual x  y)

  naoEIgual :: a -> a -> Bool
  naoEIgual x y = not (eIgual x y)

  (=:=) :: a -> a -> Bool
  x =:= y = not (x =/= y)

  (=/=) :: a -> a -> Bool
  x =/= y = not (x =:= y)


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

-- Lista
instance (Compara a) => Compara [a] where
  eIgual [] [] = True
  eIgual (x:xs) (y:ys) = eIgual x y && eIgual xs ys
  eIgual _ _ = False

  (=:=) [] [] = True
  (=:=) (x:xs) (y:ys) = (=:=) x y && (=:=) xs ys
  (=:=) _ _ = False

-- Tupla de 2
instance (Compara a, Compara b) => Compara (a,b) where
  eIgual (x,y) (a,b) = eIgual x a && eIgual y b

  (=:=) (x,y) (a,b) = (=:=) x a && (=:=) y b

-- Tupla de 3
instance (Compara a, Compara b, Compara c) => Compara (a,b,c) where
  eIgual (x,y,z) (a,b,c) = eIgual x a && eIgual y b && eIgual z c

  (=:=) (x,y,z) (a,b,c) = (=:=) x a && (=:=) y b && (=:=) z c

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