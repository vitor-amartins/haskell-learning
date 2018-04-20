isPrime :: Int -> Bool
isPrime n | n < 2 = False
          | n == 3 = True
          | otherwise = testeDiv n 2

testeDiv :: Int -> Int -> Bool
testeDiv n i | i > (raiz_quadrada n) = True
             | mod n i == 0 = False
             | otherwise = testeDiv n (i+1) 

raiz_quadrada :: Int -> Int
raiz_quadrada n = round (sqrt (fromIntegral n)) + 1

list :: IO()
list = do appendFile "Primes.txt" (replaceChar findList ',' '\n') 

findList :: String
findList = show ([2]++([a | a <- [3,5..10000000], isPrime a]))

replaceChar :: String -> Char -> Char -> String
replaceChar [] _ _ = [] 
replaceChar (a:at) old new | a == old = new : replaceChar at old new
                           | otherwise = a : replaceChar at old new