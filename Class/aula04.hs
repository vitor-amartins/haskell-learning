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

somaVetor :: (Int, Int) -> (Int, Int) -> (Int, Int)
somaVetor (x1, y1) (x2, y2) = (x1+x2, y1+y2)

moduloVetor :: (Double, Double) -> Double
moduloVetor (x, y) = sqrt(x*x + y*y)

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