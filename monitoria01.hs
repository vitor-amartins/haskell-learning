{- Vitor Martins
 - vmam@cin.ufpe.br
 -}

-------------------------------- Question 1 --------------------------------
-- Receive a list of digits and returns the sum of all numbers that are equal to the next number.
-- The list is circular.

createCircle :: [Int] -> [Int]
createCircle xs = xs++[head xs]

listEquals :: [Int] -> [Int]
listEquals [a] = []
listEquals (a:at) | (a == head at) = a : listEquals at
                  | otherwise = listEquals at

digitSum :: [Int] -> Int
digitSum xs = foldr (+) 0 (listEquals (createCircle xs))

-------------------------------- Question 2 --------------------------------
-- Receive a list of integers without repetition and returns a list of all yours permutations.



-------------------------------- Question 3 --------------------------------
-- Receive two strings and returns the positions that the first stirng appears in the second.

getIndexIn :: Char -> String -> Int -> [Int]
getIndexIn a [] n = []
getIndexIn a (b:bt) n | a == b = n: getIndexIn a bt (n+1)
                      | otherwise = getIndexIn a bt (n+1)

getAllIndex :: String -> String -> [[Int]]
getAllIndex [] _ = []
getAllIndex (a:at) b = [getIndexIn a b 0] ++ getAllIndex at b

existSequence :: [[Int]] -> Int -> Bool
existSequence [] _ = True
existSequence (a:at) n = member a n  &&  existSequence at (n+1)

existAllSequence :: [[Int]] -> Int -> [Int]
existAllSequence xs 0 | existSequence xs 0 = [0]
                      | otherwise = []
existAllSequence xs n | existSequence xs n = n:existAllSequence xs (n-1)
                      | otherwise = existAllSequence xs (n-1)

stringMatching :: String -> String -> [Int]
stringMatching a b = invertList (existAllSequence (getAllIndex a b) (length b))

-------------------------------- Question 4 --------------------------------
-- Receive two polynomials and return the multiplication of them.
-- The polynomials are represented by a list of integers.
-- The number in the position 0 corresponds to the coeficient of the 0 degree term, the number in the position 1 corresponds to the coeficient of the 1 degree term, and so on.

-- Coenficiente | 2º poli. | Indice do coeficiente | 
multP :: Int -> [Int] -> [Int]
multP _ [] = [] 
multP c (a:at) = c*a: multP c at

fixGrau :: Int -> [Int] -> Int -> [Int]
fixGrau _ [] _ = []
fixGrau c p i = zeros i ++ multP c p

-- 1º poli. | 2º poli. | Indice da vez
multAll :: [Int] -> [Int] -> Int -> [[Int]]
multAll [] _ _ =  []
multAll _ [] _ =  []
multAll (a:at) p2 i = [fixGrau a p2 i] ++ multAll at p2 (i+1)

-- Length 1º poli. | Length 2º poli. | Lista Coeficientes
fixSize :: Int -> Int -> [Int] -> [Int]
fixSize lp1 lp2 list = list ++ zeros (lp1 + lp2 - 1 - length list) 

-- fixSize no retorno de multAll
fixSizeAll :: [Int] -> [Int] -> [[Int]] -> [[Int]]
fixSizeAll _ _ [] = [] 
fixSizeAll p1 p2 (a:at) = [fixSize (length p1) (length p2) a] ++ fixSizeAll p1 p2 at

sumTwoLists :: [Int] -> [Int] -> [Int]
sumTwoLists [] [] = []
sumTwoLists (a:at) (b:bt) = a+b : sumTwoLists at bt

sumListsInList :: [[Int]] -> [Int]
sumListsInList [xs] = xs
sumListsInList (a:at) = sumListsInList ([sumTwoLists a (head at)] ++ (tail at))

poliMult :: [Int] -> [Int] -> [Int]
poliMult p1 p2 = sumListsInList (fixSizeAll p1 p2 (multAll p1 p2 0))

-- Aux --

invertList :: [t] -> [t]
invertList [] = []
invertList (a:at) =  invertList at++[a]

member :: [Int] -> Int -> Bool
member [] _ = False
member (a:at) n | a == n = True
                | otherwise = member at n

zeros :: Int -> [Int]
zeros 0 = []
zeros n = 0 : zeros (n-1)