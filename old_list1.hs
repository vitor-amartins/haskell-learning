{- Listas 1 antigas -}
import Data.List

----- 2016.2
-- 2.
twoOdd :: Int -> Int -> Int -> Bool
twoOdd a b c | (a `mod` 2 == 1) && (b `mod` 2 == 1) = True
             | (c `mod` 2 == 1) && (b `mod` 2 == 1) = True
             | (a `mod` 2 == 1) && (c `mod` 2 == 1) = True
             | otherwise = False
-- 3.
logic :: (Bool, Bool) -> (Bool, Bool, Bool)
logic (a, b) = (a && b, a || b, xor a b)

xor :: Bool -> Bool -> Bool
xor a b = not (a == b)
-- 4.
triangle :: (Int, Int, Int) -> (String, Int)
triangle (a, b, c) | c < a + b && a < b + c && b < a + c = (typeTriangle a b c, a+b+c)
                   | otherwise = ("Impossivel", 0)

typeTriangle :: Int -> Int -> Int -> String
typeTriangle a b c | (a == b) && (b == c) = "Equilatero"
                   | (a /= b) && (b /= c) && (a /= c) = "Escaleno"
                   | otherwise = "Isosceles"
-- 5.
type Point = (Float, Float, Float)

distance :: Point -> Point -> Float
distance (xa, ya, za) (xb, yb, zb) = sqrt((xb-xa)*(xb-xa) + (yb-ya)*(yb-ya) + (zb-za)*(zb-za))

type Sphere = (Point, Float)

s1 :: Sphere
s1 = ((0.0,0.0,0.0), 1.0)

isOnSurface :: Sphere -> Point -> Bool
isOnSurface ((x, y, z), r) (a, b, c) = distance (x, y, z) (a, b, c) == r

isContained :: Sphere -> Point -> Bool
isContained ((x, y, z), r) (a, b, c) = distance (x, y, z) (a, b, c) <= r
-- 6.
somaIntervalo :: Int -> Int -> Int -> Int
somaIntervalo i f m | i > f = 0
                    | otherwise = i + somaIntervalo (i+m) f m
-- 7.
sumDigits :: Int -> Int
sumDigits n | n < 10 = n
            | otherwise = n `mod` 10 + sumDigits (n `div` 10)
-- 8.
nPrime :: Int -> Int
nPrime n = primeIterations 1 2 n

primeIterations :: Int -> Int -> Int -> Int
primeIterations i x n | (i == n) && (testPrime x 2) = x
                      | testPrime x 2 =  primeIterations (i+1) (x+1) n
                      | otherwise = primeIterations i (x+1) n

testPrime :: Int -> Int -> Bool
testPrime n d | n < 2 = False
              | n `div` 2 < d = True
              | n `mod` d == 0 = False
              | otherwise = testPrime n (d+1)
-- 9.
clearTimeLine :: [String] -> [String]
clearTimeLine [] = []
clearTimeLine (a:at) | isInfixOf "Dilma" a || isInfixOf "Aecio" a = clearTimeLine at
                     | otherwise = a : clearTimeLine at
-- Remove the words Dilma and Aecio from inputs.
clearTimeLine2 :: [String] -> [String]
clearTimeLine2 list = map removeCandidates list 

removeCandidates :: String -> String
removeCandidates a = noCocaine (impeachment a)

impeachment :: String -> String
impeachment p | length p < 5 = p
impeachment (a:b:c:d:e:f) | a == 'D' && b == 'i' && c == 'l' && d == 'm' && e == 'a' = impeachment f
                          | otherwise = a : impeachment (b:c:d:e:f)

noCocaine :: String -> String
noCocaine p | length p < 5 = p
noCocaine (a:b:c:d:e:f) | a == 'A' && b == 'e' && c == 'c' && d == 'i' && e == 'o' = noCocaine f
                        | otherwise = a : noCocaine (b:c:d:e:f)
-- 10.

{- 2016.1 -}
-- 1.
bissexto :: Int -> Bool
bissexto n = n `mod` 4 == 0 && ((n `mod` 100 == 0) == (n `mod` 400 == 0))

tupla :: Int -> (Int, Int, Int)
tupla n = (n `div` 100, (n `mod` 100) `div` 10, n `mod` 10)

