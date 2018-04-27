{----------------------------- Lista 3 - 18/04 -----------------------------}
import System.Exit

-------------------------------- Question 1 --------------------------------
-- map :: (a -> b) -> [a] -> [b]
scalePointSumCoords :: Int -> (Int,Int) -> Int
scalePointSumCoords n (a,b) = n*(a+b)

f2 scalePointSumCoords
-- f2 :: (Int -> (Int,Int) -> Int) -> Int -> [(Int,Int)] -> Int
-- a = [(Int, Int)] && b = Int -> [[(Int,Int)]] -> [Int]


-- f1 :: (a -> b) -> [a] -> [b]
-- f2 :: (c -> d -> c) -> c -> [d] -> c
-- f3 :: (e -> f -> e) -> e -> [f] -> e
-- f4 :: (g -> Bool) -> [g] -> ([g],[g])
-- (+) :: (Num h) => h -> h -> h
-- (.) :: (i -> j) -> (k -> i) -> k -> j
--
-- f3.f4 :: 
-- 
--
--
--
--
--
-- f1.f2 :: ((a -> b) -> [a] -> [b]) -> ((c -> d -> c) -> c -> [d] -> c) -> k -> j
-- 
--
--
--
--
--
-- f1.f2 (+) :: 
-- 
--
--
--
--
--
-- f4.f1.f1.f2 ::
-- 
--
--
--
--
--
-------------------------------- Question 2 --------------------------------
data Temperature = Celsius Float | Fahrenheit Float | Kelvin Float

instance Show Temperature where
    show (Celsius n) = show n ++ "C" 
    show (Fahrenheit n) = show n ++ "F" 
    show (Kelvin n) = show n ++ "K"

instance Eq Temperature where
    (==) (Celsius n) (Celsius m) = n == m
    (==) (Fahrenheit n) (Fahrenheit m) = n == m
    (==) (Kelvin n) (Kelvin m) = n == m
    (==) (Celsius n) (Kelvin m) = n + 273.15 == m
    (==) (Kelvin n) (Celsius m) = Celsius m == Kelvin n
    (==) (Kelvin n) (Fahrenheit m) = (n-273)/5 == (m-32)/9
    (==) (Fahrenheit n) (Kelvin m) = Fahrenheit m == Kelvin n
    (==) (Celsius n) (Fahrenheit m) = n/5 == (m-32)/9
    (==) (Fahrenheit n) (Celsius m) = Celsius m == Fahrenheit n

instance Ord Temperature where
    (<=) (Celsius n) (Celsius m) = n <= m
    (<=) (Fahrenheit n) (Fahrenheit m) = n <= m
    (<=) (Kelvin n) (Kelvin m) = n <= m
    (<=) (Celsius n) (Kelvin m) = n + 273.15 <= m
    (<=) (Kelvin n) (Celsius m) = Celsius m >= Kelvin n
    (<=) (Kelvin n) (Fahrenheit m) = (n-273)/5 <= (m-32)/9
    (<=) (Fahrenheit n) (Kelvin m) = Fahrenheit m >= Kelvin n
    (<=) (Celsius n) (Fahrenheit m) = n/5 <= (m-32)/9
    (<=) (Fahrenheit n) (Celsius m) = Celsius m >= Fahrenheit n

minMax :: [Temperature] -> (Temperature, Temperature)
minMax (a:at) = (foldl min a at, foldl max a at)

-------------------------------- Question 3 --------------------------------

data LQueue t = LQ [t]
    deriving (Show)
data RQueue t = Empty | RQ t (RQueue t)
    deriving (Show)

instance OprQueue LQueue where
    isEmpty (LQ []) = True
    isEmpty (LQ _) = False
    enqueue (LQ a) b = (LQ (a++[b]))
    dequeue (LQ []) = error "Empty Queue!"
    dequeue (LQ (a:at)) =  (LQ at)
    peek (LQ []) = error "Empty Queue!"
    peek (LQ a) = head a

instance OprQueue RQueue where
    isEmpty (Empty) = True
    isEmpty _ = False
    enqueue (Empty) b = RQ b Empty
    enqueue (RQ a t1) b = RQ b (RQ a t1)
    dequeue (Empty) = error "Empty Queue!"
    dequeue (RQ a t1) = t1
    peek (Empty) = error "Empty Queue!"
    peek (RQ a t1) = a

class OprQueue q where
    isEmpty :: q t -> Bool
    dequeue :: q t -> q t
    enqueue :: q t -> t -> q t
    peek :: q t -> t
