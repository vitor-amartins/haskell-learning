{----------------------------- Lista 1 - 28/03 -----------------------------}

-------------------------------- Question 1 --------------------------------

kSmallest :: [Int] -> Int -> [Int]
kSmallest [] _ = []
kSmallest _ 0 = []
kSmallest l n | length l == n = l
                   | otherwise = kSmallest (removeElement l (maxList l)) n

maxList :: [Int] -> Int
maxList [a] = a
maxList (a:at) = max a (maxList at)

removeElement :: [Int] -> Int -> [Int]
removeElement [] _ = []
removeElement (a:at) n | a == n = at
                       | otherwise = a : removeElement at n

-------------------------------- Question 2 --------------------------------

composites :: [Int] -> [Int] -> [Int]
composites _ [] = []
composites a (b:bt) | valorDivisivel a b = b : composites a bt
                    | otherwise = composites a bt

valorDivisivel :: [Int] -> Int -> Bool
valorDivisivel [] _ = False
valorDivisivel _ 1 = True
valorDivisivel (a:at) b | b `mod` a == 0 = valorDivisivel (a:at) (b `div` a)
                        | otherwise = valorDivisivel at b

-------------------------------- Question 3 --------------------------------

comb :: [Int] -> [[Int]]
comb [] = [[]]
comb [a] = [[a]] ++ [[]]
comb (a:at) = [[a]] ++ [(a:at)] ++ firstWithOthers a at ++ comb at

firstWithOthers :: Int -> [Int] -> [[Int]]
firstWithOthers _ [] = []
firstWithOthers a (b:bt) = [[a] ++ [b]] ++ firstWithOthers a bt

unique :: [[Int]] -> [[Int]]
unique [[]] = [[]]
unique (a:at) = a : filter (\x -> x /= a) (unique at)

comb2 :: [Int] -> Int -> [[Int]]
comb2 [] _ = [[]]
comb2 _ 0 = [[]]
comb2 l n = comb l ++ comb2 (rollVector l) (n-1)

combinations :: [Int] -> [[Int]]
combinations [] = [[]]
combinations l = unique (map quickSort (comb2 l (length l)))

rollVector :: [Int] -> [Int]
rollVector [] = []
rollVector (a:at) = at ++ [a]

quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (a:at) = quickSort [x | x <- at, x <= a]++ [a] ++ quickSort [x | x <- at, x > a]