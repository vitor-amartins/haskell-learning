{- Lista 1 - 28/03 -}

-------------------------------- Question 1 --------------------------------

kSmallest :: [Int] -> Int -> [Int]
kSmallest [] _ = []
kSmallest _ 0 = []
kSmallest l n = 