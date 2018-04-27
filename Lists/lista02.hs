{----------------------------- Lista 2 - 04/04 -----------------------------}

-------------------------------- Question 1 --------------------------------
delta = 0.000001

newton :: (Double -> Double) -> Double -> Double -> Double
newton f x eps | absol (fOverf' f x) < eps = x - (fOverf' f x)
               | otherwise = newton f (x - (fOverf' f x)) eps

fOverf' :: (Double -> Double) -> Double -> Double
fOverf' f x = (f x)/(f' f x)

f' :: (Double -> Double) -> Double -> Double
f' f x = (f (x + delta) - f x)/delta

absol :: Double -> Double
absol x | x > 0 = x
        | otherwise = -1*x

-- Funções para teste
ftest :: Double -> Double
ftest x = x*x - 1

ftest2 :: Double -> Double
ftest2 x = x*x -3*x + 2

-------------------------------- Question 2 --------------------------------

--- A) ---
thrice :: (a -> a) -> a -> a
thrice f x = f (f (f x))
{-

(.) thrice map

(.) :: (b -> c) -> (a -> b) -> (a -> c)

thrice :: (t -> t) -> t -> t

map :: (u -> v) -> [u] -> [v]

a :: (u -> v)
b :: [u] -> [v]

b :: (t -> t)
c :: t -> t

Mas para b ser válido temos que:
t -> t == [u] -> [v] => t == [u] && t == [v] => [u] == [v]
t -> t == [u] -> [u]

(.) thrice map :: (a -> c)
(.) thrice map :: (u -> u) -> (t -> t) 
-- Como t == [u], temos:
(.) thrice map :: (u -> u) -> ([u] -> [u]) 

-}

--- B) ---
swap :: a -> (a -> b) -> b
swap f g = g f
{-

swap map thrice

swap :: a -> (a -> b) -> b

thrice :: (t -> t) -> t -> t

map :: (u -> v) -> [u] -> [v]

a1 :: (u -> v) -> [u] -> [v]

a2 :: (t -> t) -> t
b2 :: t
OU
a3 :: (t -> t)
b3 :: t -> t

Precisamos que a1 == a2 || a1 == a3
Mas (u -> v) -> [u] -> [v] /= (t -> t) -> t && (u -> v) -> [u] -> [v] /= (t -> t)

Logo não é possível fazer essa composição.

-}

--- C) ---

{-

(.) :: (b -> c) -> (a -> b) -> (a -> c)

head :: [t] -> t
-------  a   -> b
tail :: [ts] -> [ts]
-------  b   ->  c
tail . head :: ([ts] -> [ts]) -> ([t] -> ts) -> ([t] -> [ts])
-------------- ( b  ->  c ) -> ( a  -> b) -> ( a  ->  c )
tail . head :: [t] -> [ts]

Para isso ser possível temos que t deve ser uma lista,
neste caso t :: [ts]
logo [t] :: [[ts]]
Podemos reescrever da seguinte forma:
tail . head :: [[ts]] -> [ts]
Essa função retorna a cauda da primeira lista de [t]

-}

-------------------------------- Question 3 --------------------------------

a = [["The Shape of Water", "Dunkirk", "Get Out"],
     ["Get Out", "Dunkirk", "The Shape of Water"],
     ["Dunkirk", "The Shape of Water", "Get Out"]]

a1 = [["A", "B", "C", "D"],
      ["A", "B", "D", "C"]]

a2 = [["A", "B", "C", "D"],
      ["A", "B", "D", "C"],
      ["B", "A", "D", "C"],
      ["B", "A", "D", "C"],
      ["D", "C", "B", "A"]]

a3 = [["A", "B", "C", "D"],
      ["A", "B", "D", "C"],
      ["B", "A", "D", "C"],
      ["D", "B", "A", "C"],
      ["D", "B", "A", "C"]]
-- Pegar uma lista com os filmes
getListMovies :: [[String]] -> [String] -- head input
getListMovies (a:at) = a
-- Qnts filmes tem, i.e., quantas posições possíveis
posMax :: [String] -> Int
posMax l = (length l) - 1

vezesPosN :: [String] -> [[String]] -> Int -> [(String, Int)]
vezesPosN [] _ _ = []
vezesPosN (a:at) l n = (a, vfn a l n) : vezesPosN at l n

vfn :: String -> [[String]] -> Int -> Int
vfn movie [] n = 0
vfn movie (a:at) n | movie == (getOnIndex a n 0) = 1 + vfn movie at n
                   | otherwise = vfn movie at n

-- pega item no indice lista | indice pra pegar | 0 
getOnIndex :: [t] -> Int -> Int -> t
getOnIndex (a:at) i n | i == n = a
                      | otherwise = getOnIndex at i (n+1)

findMin :: [(String, Int)] -> Int
findMin [a] = snd a
findMin (a:at) = min (snd a) (findMin at)

findMax :: [(String, Int)] -> Int
findMax [] = -1
findMax (a:at) = max (snd a) (findMax at)

-- list | max | min
removeWorst :: [(String, Int)] -> Int -> Int -> [(String, Int)]
removeWorst [] _ _ = []
removeWorst (a:at) maximo minimo | maximo == minimo = (a:at)
                                 | (snd a) == minimo = removeWorst at maximo minimo
                                 | otherwise = a : removeWorst at maximo minimo
-- listMovies | moviesAndQntPos in N | N
doIt :: [[String]] -> [String] -> [(String, Int)] -> Int -> [(String, Int)]
doIt listP listM [] n = []
doIt listP listM c n | (length c) == 1 = c
                   | c == (removeWorst c (findMax c) (findMin c)) = doIt listP (getNewMovies c) (vezesPosN (getNewMovies c) listP (n+1)) (n+1) --Pegar nova lista de filmes
                   | otherwise = doIt listP listM (removeWorst c (findMax c) (findMin c)) n

getWinner :: [(String, Int)] -> String
getWinner (a:at) = fst a

getNewMovies :: [(String, Int)] -> [String]
getNewMovies [] = []
getNewMovies (a:at) = (fst a):getNewMovies at

winner :: [[String]] -> String
winner a = getWinner (doIt a (getListMovies a) (vezesPosN (getListMovies a) a 0) 0)

-----
