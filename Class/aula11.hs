{- Aula 11 - 10/04 -}

data Distance = Km Float | Miles Float

data Set t = Set [t]

-- 1) Definir instâncias das classes Eq, Show para os tipos de dados acima
-- 2) Definir instância da classe Ord para o tipo instância
-- 3) Conjunto sem repetição
-- 4) Comparação de conjuno não leva em conta tipo nem ordem

instance Show Distance where
    show (Km v) = "*** " ++ show v ++ " Km ***"
    show (Miles v) = "*** " ++ show v ++ " Mi ***"

instance Eq Distance where
    (==) (Km v) (Km u) = v == u  
    (==) (Miles v) (Miles u) = v == u  
    (==) (Km k) (Miles m) = (1.609)*k == m
    (==) (Miles m) (Km k) = Km k == Miles m

instance Ord Distance where
    (<=) (Km v) (Km u) = v <= u
    (<=) (Miles v) (Miles u) = v <= u
    (<=) (Km k) (Miles m) = k*(1.609) <= m
    (<=) (Miles m) (Km k) = Km k >= Miles m

unique :: Eq t => [t] -> [t]
unique [] = []
unique (x:xs) = x: unique[a | a <- xs, a /= x]

b = Set [2,1,3,4]

-- instance  Show Set where
--     show (Set [t]) = "Empty" 