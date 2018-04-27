answer :: Int
answer = 42

square :: Int -> Int
square x = x * x

allEqual :: Int -> Int -> Int -> Bool
allEqual n m p = (n == m) && (m == p)

maxi :: Int -> Int -> Int
maxi n m | n >= m = n
         | otherwise = m

doubleSmallNumber :: Int -> Int
doubleSmallNumber x | x > 100 = x
                    | otherwise = x*2

fat :: Int -> Int
fat 0 = 1
fat n = n * fat (n-1)

all4Equal :: Int -> Int -> Int -> Int -> Bool
all4Equal a b c d = (a == b) && (b == c) && (c == d)

eqaulCount :: Int -> Int -> Int -> Int
eqaulCount a b c | allEqual n m p = 3
                 | (a /= b) && (b /= c) && (c /= a) = 0
                 | otherwise = 2