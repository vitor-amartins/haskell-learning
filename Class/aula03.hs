fat :: Int -> Int
fat 0 = 1
fat n = fat (n-1) * n

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

somaVetores :: (Int, Int) -> (Int, Int) -> (Int, Int)
somaVetores (x1, y1) (x2, y2) = (x1+x2, y1+y2)

moduloVetor :: (Double, Double) -> Double
moduloVetor (x, y) = sqrt(x*x + y*y)

{- Aula 03 -}

addEspacos :: Int -> String
addEspacos 0 = ""
addEspacos n = " " ++ addEspacos (n-1)

menorMaior :: Int -> Int -> Int -> (Int, Int)
menorMaior a b c | (a >= b) && (a >= c) && (b >= c) = (c, a)
                 | (a >= b) && (a >= c) && (c >= b) = (b, a)
                 | (b >= a) && (b >= c) && (a >= c) = (c, b)
                 | (b >= a) && (b >= c) && (c >= a) = (a, b)
                 | (c >= a) && (c >= b) && (a >= b) = (b, c)
                 | (c >= a) && (c >= b) && (b >= a) = (a, c)

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

