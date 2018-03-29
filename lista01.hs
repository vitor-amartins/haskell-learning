{- Lista 1 - 28/03 -}

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

combinations :: [Int] -> [[Int]]
combinations [] = [[]]
combinations [a] = [[a]] ++ [[]]
combinations (a:at) = [[a]] ++ [(a:at)] ++ firstWithOthers a at ++ combinations at

firstWithOthers :: Int -> [Int] -> [[Int]]
firstWithOthers _ [] = []
firstWithOthers a (b:bt) = [[a] ++ [b]] ++ firstWithOthers a bt

unique :: [[Int]] -> [[Int]]
unique [[]] = [[]]
unique (a:at) = a : unique [x | x <- at, x /= a]    