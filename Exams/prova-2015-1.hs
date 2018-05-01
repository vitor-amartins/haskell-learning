type Resultado = [Int]
type Jogos = [[Int]]
type Jogo = [Int]

r1 :: Resultado
r1 = [4,6,8,2,1,3]

j1 :: Jogo
j1 = [5,6,8,2,1,10]

j2 :: Jogo
j2 = [4,6,8,2,1,7]

j3 :: Jogo
j3 = [4,6,8,2,1,3]

j4 :: Jogo
j4 = [4,6,8,2,1,3]

jg :: Jogos
jg = [j1,j2,j3,j4]

premiados :: Resultado -> Jogos -> Int
premiados r [] = 0
premiados r (a:at) | winner r a = 1 + premiados r at
                   | otherwise = premiados r at

winner :: Resultado -> Jogo -> Bool
winner [a] j | elem a j = True
             | otherwise = False
winner (a:at) j | elem a j = winner at j
                | otherwise = False

acertos :: Resultado -> Jogos -> [Int]
acertos r [] = []
acertos r (a:at) = (acertosCartao r a):(acertos r at)

acertosCartao :: Resultado -> Jogo -> Int
acertosCartao [a] j | elem a j = 1
                    | otherwise = 0
acertosCartao (a:at) j | elem a j = 1 + acertosCartao at j
                       | otherwise = acertosCartao at j

numPremios :: Resultado -> Jogos -> (Int, Int, Int)
numPremios r j = (countElem 4 (acertos r j), countElem 5 (acertos r j), countElem 6 (acertos r j))

countElem :: (Eq t) => t -> [t] -> Int
countElem n [] = 0
countElem n (a:at) | n == a = 1 + countElem n at
                   | otherwise = countElem n at