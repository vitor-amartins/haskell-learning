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