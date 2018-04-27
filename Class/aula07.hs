fatorial :: Integer -> Integer
fatorial 0 = 1
fatorial n = fatorial (n-1) * n

all3Equal :: Int -> Int -> Int -> Bool
all3Equal a b c = (a == b) && (b == c)

all4Equal :: Int -> Int -> Int -> Int -> Bool
all4Equal a b c d = (a == b) && all3Equal b c d

equalCount :: Int -> Int -> Int -> Int 
equalCount a b c  | all3Equal a b c  = 3
                  | (a /= b) && (b /= c) && (a /= c) = 0
                  | otherwise = 2

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci(n-2)

somaVetor :: (Double, Double) -> (Double, Double) -> (Double, Double)
somaVetor (x1, y1) (x2, y2) = (x1+x2, y1+y2)

moduloVetor :: (Double, Double) -> Double
moduloVetor (x, y) = sqrt(x*x + y*y)

moduloVetorPontos :: (Double, Double) -> (Double, Double) -> Double
moduloVetorPontos (x1, y1) (x2, y2) = sqrt((x1-x2)*(x1-x2) + (y1-y2)*(y1-y2))

{- Aula 03 -}

addEspacos :: Int -> String
addEspacos 0 = ""
addEspacos n = " " ++ addEspacos (n-1)

menorMaior :: Int -> Int -> Int -> (Int, Int)
menorMaior a b c = (min (min a b) c, max (max a b) c)

ordenaTripla :: (Int, Int, Int) -> (Int, Int, Int)
ordenaTripla (a, b, c) | (a >= b) && (a >= c) && (b >= c) = (c, b, a)
                       | (a >= b) && (a >= c) && (c >= b) = (b, c, a)
                       | (b >= a) && (b >= c) && (a >= c) = (c, a, b)
                       | (b >= a) && (b >= c) && (c >= a) = (a, c, b)
                       | (c >= a) && (c >= b) && (a >= b) = (b, a, c)
                       | (c >= a) && (c >= b) && (b >= a) = (a, b, c)

type Ponto = (Double, Double)
type Reta = (Ponto, Ponto)

getX :: Ponto -> Double
getX p = fst(p)

getY :: Ponto -> Double
getY p = snd(p)

isHorizontal :: Reta -> Bool
isHorizontal r = getY(fst(r)) == getY(snd(r))

isVertical :: Reta -> Bool
isVertical r = getX(fst(r)) == getX(snd(r))

par :: Int -> Bool
par n = mod n 2 == 0

{- Listas -}

sumList :: [Int] -> Int
sumList [] = 0
sumList (a:at) = a + sumList at

doubleList :: [Int] -> [Int]
doubleList [] = []
doubleList (a:at) = [a*2]++doubleList at

member :: [Int] -> Int -> Bool
member [] _ = False
member (a:at) n | a == n = True
                | otherwise = member at n

digits :: String -> String
digits [] = []
digits (a:at) | a >= '0' && a <= '9' = a:digits at
              | otherwise = digits at

sumListPares :: [(Int, Int)] -> [Int]
sumListPares [] = []
sumListPares (a:at) = (fst(a)+snd(a)):sumListPares at 

{- Extra -}

sizeList :: [t] -> Int
sizeList [] = 0
sizeList (a:at) = 1 + sizeList at

equalList :: [Int] -> [Int] -> Bool
equalList [] [] = True
equalList [] _ = False
equalList _ [] = False
equalList (a:at) (b:bt) | (a == b) && equalList at bt = True
                        | otherwise = False

invertList :: [t] -> [t]
invertList [] = []
invertList (a:at) =  invertList at++[a]

maiorList :: [Int] -> Int
maiorList [x] = x
maiorList (a:at) = max a (maiorList at)

paresList :: [Int] -> [Int]
paresList [x] | par x = [x]
              | otherwise = []
paresList (a:at) | par a = [a]++paresList at
                 | otherwise = paresList at

imparesList :: [Int] -> [Int]
imparesList [x] | not (par x) = [x]
                | otherwise = []
imparesList (a:at) | not (par a) = [a]++imparesList at
                   | otherwise = imparesList at

{- Aula4 - 15/03 -}

rep :: Int -> t -> [t]
rep 0 ch = []
rep n ch = ch : rep (n-1) ch

type Pessoa = String
type Livro = String
type BancoDados = [(Pessoa, Livro)]

baseExemplo :: BancoDados
baseExemplo = [("Sergio","O Senhor dos Aneis"),("Andre","Duna"),("Fernando","Jonathan Strange & Mr.Norrell"),("Fernando","Duna")]

livros :: BancoDados -> Pessoa -> [Livro]
livros [] nome = []
livros (a:at) nome | fst(a) == nome = snd(a):livros at nome
                   | otherwise = livros at nome 

emprestimos :: BancoDados -> Livro -> [Pessoa]
emprestimos [] lv = []
emprestimos (a:at) lv | snd(a) == lv = fst(a):emprestimos at lv
                      | otherwise = emprestimos at lv 

emprestado :: BancoDados -> Livro -> Bool
emprestado [] lv = False
emprestado (a:at) lv | snd(a) == lv = True
                     | otherwise = emprestado at lv

qntEmprestimos :: BancoDados -> Livro -> Int
qntEmprestimos [] lv = 0
qntEmprestimos (a:at) lv | snd(a) == lv = 1 + qntEmprestimos at lv
                         | otherwise = qntEmprestimos at lv

emprestar :: BancoDados -> Pessoa -> Livro -> BancoDados
emprestar bd pss lv = (pss, lv):bd 

devolver :: BancoDados -> Pessoa -> Livro -> BancoDados
devolver [] _ _ = []
devolver (a:at) pss lv | pss == fst(a) && lv == snd(a) = at
                       | otherwise = a:devolver at pss lv

{- Aula 06 - 20/03 -}

memberComp :: [Int] -> Int -> Bool
memberComp li n = [x | x <- li, x == n] /= []

livrosComp :: BancoDados -> Pessoa -> [Livro]
livrosComp bd nome = [liv | (pess, liv) <- bd, pess == nome]

quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (a:at) = quickSort [x | x <- at, x <= a]++ [a] ++ quickSort [x | x <- at, x > a]

insert :: Int -> [Int] -> [Int]
insert n li = [x | x <- li, x <= n]++[n]++[x | x <- li, x > n]

insertSort :: [Int] -> [Int]
insertSort [] = []
insertSort (a:at) = insert a (insertSort at)

unique :: [Int] -> [Int]
unique [] = []
unique (a:at) = a:unique[x | x <- at, x /= a]

{- Aula 07 - 22/03 -}

dobro :: Int -> Int
dobro x = 2*x

quadrado :: Int -> Int
quadrado x = x*x

fun :: Int -> Int
fun x = -x*x + x*4 + 1

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

--totalVendas :: Int -> Int
--totalVendas 0 = vendas 0
--totalVendas n = vendas n + totalVendas (n-1)

sumSquares :: Int -> Int
sumSquares 0 = quadrado 0
sumSquares n = quadrado n + sumSquares (n-1)

somatorio :: (Int -> Int) -> Int -> Int
somatorio f 0 = f 0
somatorio f n = f n + somatorio f (n-1)

sumSquares' :: Int -> Int
sumSquares' n = somatorio quadrado n

minFun :: (Int -> Int) -> Int -> Int
minFun f 0 = f 0
minFun f n = min (f n) (minFun f (n-1))

maxFun :: (Int -> Int) -> Int -> Int
maxFun f 0 = f 0
maxFun f n = max (f n) (maxFun f (n-1))

isCrescent :: (Int -> Int) -> Int -> Bool
isCrescent f 1 = f 1 - f 0 >= 0
isCrescent f n = (f n - f (n-1) >= 0) && isCrescent f (n-1)

quadradoLista xs = map quadrado xs

-- somaQuadradosLista xs = foldr1 quadradoLista xs