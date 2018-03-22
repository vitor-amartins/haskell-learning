{-  -}

-- Question 1 --
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

-- Question 2 --
-- Receive a list of integers without repetition and returns a list of all yours permutations.

-- Question 3 --
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

-- Question 4 --
{- Receive two polynomials and return the multiplication of them.
   The polynomials are represented by a list of integers,
   the number in the position 0 corresponds to the coeficient of the 0 degree term,
   the number in the position 1 corresponds to the coeficient of the 1 degree term, and so on. 
-}


-- Aux --

invertList :: [t] -> [t]
invertList [] = []
invertList (a:at) =  invertList at++[a]

member :: [Int] -> Int -> Bool
member [] _ = False
member (a:at) n | a == n = True
                | otherwise = member at n