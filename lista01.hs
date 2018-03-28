{- Lista 1 - 28/03 -}

-------------------------------- Question 1 --------------------------------

--kSmallest :: [Int] -> Int -> [Int]
--kSmallest [] _ = []
--kSmallest _ 0 = []
--kSmallest (a:at) n = 

minList :: [Int] -> Int
minList [a] = a
minList (a:at) = min a (minList at)

removeElement :: [Int] -> Int -> [Int]
removeElement [] _ = []
removeElement (a:at) n | a == n = at
					   | otherwise = a :removeElement at 