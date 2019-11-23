-- Mauro Hemerly (Hämmerli) Gazzani
-- Disciplina de Programação Funcional
-- Curso de Engenharia de Computação
-- UEMG - Unidade de Ituiutaba


-- Sinônimos de Tipos
type Nome = String
type SobreNome = String
type Altura = Float
type Peso = Int
type PacienteT = (Nome, SobreNome,Altura,Peso)

nomePaciente :: PacienteT -> Nome
nomePaciente (nome,sobreN,altura,peso) = nome

sobreNomePaciente :: PacienteT -> SobreNome
sobreNomePaciente (nome,sobreN,altura,peso) = sobreN

alturaPaciente :: PacienteT -> Altura
alturaPaciente (nome,sobreN,altura,peso) = altura

pesoPaciente :: PacienteT -> Peso
pesoPaciente (nome,sobreN,altura,peso) = peso


-- Definição de Novos Tipos de Dados

data Sexo = Feminino | Masculino deriving Show

abreviaturaSexo :: Sexo -> Char
abreviaturaSexo Feminino = 'F'
abreviaturaSexo Masculino = 'M'

-- Tipagem sanguínea 
data TipoRH = Positivo | Negativo deriving Show

data TipoABO = A | B | AB | O deriving Show

data TipoSangue = TipoSangue TipoABO TipoRH deriving Show

showRh :: TipoRH -> String
showRh Positivo = "+"
showRh Negativo = "-"

showABO :: TipoABO -> String
showABO A = "A"
showABO B = "B"
showABO AB = "AB"
showABO O = "O"

showTipoSanguineo :: TipoSangue -> String
showTipoSanguineo (TipoSangue abo rh) = showABO abo ++ showRh rh


quemPodeDoar :: TipoSangue -> TipoSangue -> Bool
quemPodeDoar (TipoSangue O _) _ = True    -- Doador universal
quemPodeDoar _ (TipoSangue AB _) = True   -- Receptor universal
quemPodeDoar (TipoSangue A _) (TipoSangue A _) = True
quemPodeDoar (TipoSangue B _) (TipoSangue B _) = True
quemPodeDoar _ _ = False  

paciente1 = TipoSangue A Positivo
paciente2 = TipoSangue O Negativo
paciente3 = TipoSangue AB Positivo

data Paciente = Paciente Nome SobreNome Altura Peso Idade Sexo TipoSangue deriving Show
 

getNome :: Paciente -> Nome
getNome (Paciente nome _ _ _ _ _ _) = nome

getIdade :: Paciente -> Idade
getIdade (Paciente _ _ _ _ idade _ _) = idade

getSexo :: Paciente -> Sexo
getSexo (Paciente _ _ _ _ _ sexo _) = sexo

getTipoSangue :: Paciente -> TipoSangue
getTipoSangue (Paciente _ _ _ _ _ _ sangue) = sangue

data Cor = Vermelho | Verde | Azul deriving Eq

data Mes = Jan | Fev | Mar | Abr | Mai | Jun | Jul | Ago | Set | Out | Nov | Dez

data Endereco = Rua String Int Complemento
data Complemento = Casa | Apartamento

type Idade = Int
type Salario = Float

data Funcionario = Executivo Nome Idade Salario | 
                   VicePresidente String Idade Salario |
                   Gerente Nome Idade Salario |
                   Engenheiro Nome Idade Salario


-- Árvore Binária
data ArvoreBinaria a = No (ArvoreBinaria a) a (ArvoreBinaria a) | Null
  deriving Show

pertence :: Eq a => ArvoreBinaria a -> a -> Bool
pertence Null _ = False
pertence (No esq e dir) elem 
        | e == elem = True
        | otherwise = pertence esq elem || pertence dir elem

-- Árvore de Busca Binária
insereArvore :: Ord a => ArvoreBinaria a -> a -> ArvoreBinaria a
insereArvore Null elem = No Null elem Null
insereArvore (No esq e dir) elem
        | elem > e = No esq e (insereArvore dir elem) 
        | elem < e = No (insereArvore esq elem) e dir
        | otherwise = No esq e dir

preorder ::  ArvoreBinaria a -> [a]
preorder Null = []
preorder (No esq e dir) = [e] ++ preorder esq ++ preorder dir

inorder :: ArvoreBinaria a -> [a]
inorder Null = []
inorder (No esq e dir) = inorder esq ++ [e] ++ inorder dir

postorder :: ArvoreBinaria a -> [a]
postorder Null = []
postorder (No esq e dir) = postorder esq ++ postorder dir ++ [e]



data Expr = Literal Int | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr
  deriving (Show,Eq)

avalia :: Expr -> Int
avalia (Literal x) = x
avalia (Add expr1 expr2) = avalia expr1 + avalia expr2
avalia (Sub expr1 expr2) = avalia expr1 - avalia expr2
avalia (Mul expr1 expr2) = avalia expr1 * avalia expr2
avalia (Div expr1 expr2) = avalia expr1 `div` avalia expr2

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

--iguala :: NomeCompleto -> NomeCompleto -> Bool
--iguala (Nome x) (Nome y) = x == y


--data NomeCompleto = Nome String deriving Eq
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