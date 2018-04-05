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

-------------------------------- Question 3 --------------------------------

a = [["The Shape of Water", "Dunkirk", "Get Out"],
     ["Get Out", "Dunkirk", "The Shape of Water"],
     ["Dunkirk", "The Shape of Water", "Get Out"]]

-- Pegar uma lista com os filmes
getListMovies :: [[String]] -> [String] -- head input
getListMovies (a:at) = a
-- Qnts filmes tem, i.e., quantas posições possíveis
qntPos :: [String] -> Int
qntPos l = length l
-- (Filme, vetor qntd de vezes q apareceu na posição do índice)
-- ("Get Out", [1, 0, 2])
-- ("Dunkirk", [1, 2, 0])
-- ("The Shape of Water", [1, 1, 1])
vezesPos :: [String] -> [[String]] -> [(String, [Int])]




first :: [[String]] -> [[(String, Int)]]
first (a:at) = map zipList (a:at)

zipList :: [String] -> [(String, Int)]    
zipList l = zip l [0..]


movP :: [String] -> [[(String, Int)]] -> [(String, [Int])]
movP [] _ = []
movP (a:at) l = (a, moviePositions l a) : movP at l

moviePositions :: [[(String, Int)]] -> String -> [Int]
moviePositions [] _ = []
moviePositions (a:at) movie = [movieSum2 a movie] ++ (moviePositions at movie)

movieSum2 :: [(String, Int)] -> String -> Int
movieSum2 [] _ = 0
movieSum2 (a:at) movie | movie == fst a = snd a
                       | otherwise = movieSum2 at movie

countPositions :: (String, [Int]) -> Int -> Int
countPositions (movie, []) _ = 0
countPositions (movie, (a:at)) n | a == n = 1 + countPositions (movie, at) n
                                 | otherwise = countPositions (movie, at) n