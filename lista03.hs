{----------------------------- Lista 3 - 18/04 -----------------------------}

-------------------------------- Question 1 --------------------------------

-------------------------------- Question 2 --------------------------------
data Temperature = Celsius Float | Fahrenheit Float | Kelvin Float

instance Show Temperature where
    show (Celsius n) = show n ++ "C" 
    show (Fahrenheit n) = show n ++ "F" 
    show (Kelvin n) = show n ++ "K"

instance Eq Temperature where
    (==) (Celsius n) (Celsius m) = n == m
    (==) (Fahrenheit n) (Fahrenheit m) = n == m
    (==) (Kelvin n) (Kelvin m) = n == m
    (==) (Celsius n) (Kelvin m) = n + 273.15 == m
    (==) (Kelvin n) (Celsius m) = Celsius m == Kelvin n
    (==) (Kelvin n) (Fahrenheit m) = (n-273)/5 == (m-32)/9
    (==) (Fahrenheit n) (Kelvin m) = Fahrenheit m == Kelvin n
    (==) (Celsius n) (Fahrenheit m) = n/5 == (m-32)/9
    (==) (Fahrenheit n) (Celsius m) = Celsius m == Fahrenheit n

instance Ord Temperature where
    (<=) (Celsius n) (Celsius m) = n <= m
    (<=) (Fahrenheit n) (Fahrenheit m) = n <= m
    (<=) (Kelvin n) (Kelvin m) = n <= m
    (<=) (Celsius n) (Kelvin m) = n + 273.15 <= m
    (<=) (Kelvin n) (Celsius m) = Celsius m >= Kelvin n
    (<=) (Kelvin n) (Fahrenheit m) = (n-273)/5 <= (m-32)/9
    (<=) (Fahrenheit n) (Kelvin m) = Fahrenheit m >= Kelvin n
    (<=) (Celsius n) (Fahrenheit m) = n/5 <= (m-32)/9
    (<=) (Fahrenheit n) (Celsius m) = Celsius m >= Fahrenheit n

minMax :: [Temperature] -> (Temperature, Temperature)
minMax (a:at) = (foldl min a at, foldl max a at)

-------------------------------- Question 3 --------------------------------
