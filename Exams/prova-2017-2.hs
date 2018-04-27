--
--Primeira Prova de Paradigmas de Linguagens Computacionais 
-- 2/2017 - 10/10/2017
--
-- Nome: 
--

-- 1) (2.0) Escreva uma funcao locate, que recebe como entrada um elemento e uma lista de elementos, e retorna a localização (o índice) daquele elemento dentro da lista. 
-- A primeira posição na lista tem índice 0 (zero).
-- Caso o elemento não pertença à lista, deve ser retornado o valor (-1).
-- Exemplos: locate 'x' "abcdewxyz" ------>  6
--           locate 5   [5,98,7,32] ------>  0
--           locate True [False, False] --> -1
-- locate :: Eq t => t -> [t] -> Int

locate :: Eq t => t -> [t] -> Int
locate n list = find n list 0

find :: Eq t => t -> [t] -> Int -> Int
find _ [] _ = -1
find n (a:at) i | n == a = i
                | otherwise = find n at (i+1) 

-- 2) (3.0) Escreva uma função que verifique se uma lista está contida em outra (por exemplo, se uma String é substring de outra).
-- Exemplos: substr "abc" "xyz12abrt" ----> False
--           substr "abc" "aaabrsabcfr" --> True
--           substr "aab" "aacrtxxeaayb" -> False
-- substr :: String -> String -> Bool
--substr :: String -> String -> Bool

substr :: String -> String -> Bool
substr _ [] = False
substr (a:at) (b:bt) | (locate a (b:bt)) == -1 = False
                      | (getNFst (length (a:at)) (getSubAt (locate a (b:bt)) (b:bt))) == (a:at) = True
                      | otherwise = substr (a:at) bt

getSubAt :: Int -> String -> String
getSubAt _ [] = []
getSubAt n (a:at) | n == 0 = a:at
                  | otherwise = getSubAt (n-1) at

getNFst :: Int -> String -> String
getNFst 0 _ = [] 
getNFst _ [] = []
getNFst n (a:at) | n == 0 = [a]
                 | otherwise = a : getNFst (n-1) at

-- 3) Um robô é controlado por 4 comandos: 
--    Left, para girar sua direção à esquerda 90 graus;
--    Right, para girar sua direção à direita em 90 graus;
--    Forward seguido de um número N, que indica um avanço de N metros.
--    Backward seguido de um número N, que indica um retrocesso de N metros.

-- Supondo que o robô comece na posição (0,0) (coordenadas) e direcionado para norte (i.e. para o posição (0,1)): 
-- (3.0) faça uma função destination que informe a localização do robô após uma sequêcia de comandos.

-- Exemplo de posições/coordenadas:
-- (-2, 2) (-1, 2) (0, 2) (1, 2) (2, 2)
-- (-2, 1) (-1, 1) (0, 1) (1, 1) (2, 1)
-- (-2, 0) (-1, 0) (0, 0) (1, 0) (2, 0)
-- (-2,-1) (-1,-1) (0,-1) (1,-1) (2,-1)
-- (-2,-2) (-1,-2) (0,-2) (1,-2) (2,-2)

data Command = Forward Int | Backward Int | TurnLeft |  TurnRight 
  deriving (Eq, Show)
data Direction = North | South | West | East
  deriving (Show)

-- exemplo: destination (0,0) [Forward 2, TurnLeft, TurnLeft, Forward 1] ---> (0,1)
--          destination (0,0) [Backward 2, Forward 1] ---> (0,-1)
-- destination :: (Int,Int) -> [Command] -> (Int,Int)

destination :: (Int,Int) -> [Command] -> (Int,Int)
destination (x,y) list = move (x,y) list North

move :: (Int, Int) -> [Command] -> Direction -> (Int, Int)
move (x,y) [] _ = (x,y)
move (x,y) (a:at) North | a == TurnLeft = move (x,y) at West
                        | a == TurnRight = move (x,y) at East
                        | otherwise = move (x,(y+(distance a))) at North
move (x,y) (a:at) East  | a == TurnLeft = move (x,y) at North
                        | a == TurnRight = move (x,y) at South
                        | otherwise = move ((x+(distance a)),y) at East
move (x,y) (a:at) South | a == TurnLeft = move (x,y) at East
                        | a == TurnRight = move (x,y) at West
                        | otherwise = move (x,(y-(distance a))) at South
move (x,y) (a:at) West  | a == TurnLeft = move (x,y) at South
                        | a == TurnRight = move (x,y) at North
                        | otherwise = move ((x-(distance a)),y) at West

distance :: Command -> Int
distance (Forward x) = x
distance (Backward x) = -x

-- 4) (2.0) faça uma função faces que informe para qual direção o robô estará voltado ao final de uma sequência de comandos (North, South, East ou West), assumindo que ele começa voltado para a direção North.
-- exemplo: faces North [Forward 2, TurnLeft, TurnLeft, Forward 1] ---> South
--          faces North [Backward 2, Forward 1] ---> North
--          faces North [TurnLeft, TurnLeft, TurnLeft] ---> East
-- faces ::  Direction -> [Command] -> Direction

faces ::  Direction -> [Command] -> Direction
faces n [] = n
faces North (a:at) | a == TurnLeft = faces West at
                   | a == TurnRight = faces East at
                   | otherwise = faces North at
faces East (a:at)  | a == TurnLeft = faces North at
                   | a == TurnRight = faces South at
                   | otherwise = faces East at
faces South (a:at) | a == TurnLeft = faces East at
                   | a == TurnRight = faces West at
                   | otherwise = faces South at
faces West (a:at)  | a == TurnLeft = faces South at
                   | a == TurnRight = faces North at
                   | otherwise = faces West at