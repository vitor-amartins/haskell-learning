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