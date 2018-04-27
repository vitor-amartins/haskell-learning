-- Primeira Prova de Paradigmas de Linguagens Computacionais
-- 2/2016 - 29/09/2016
--
-- Nome:
--
{- O formato CSV (comma-spearated value) é bastante utilizado para
disponibilizar dados abertos ou para disponiblizar arquivos com
logs.
 Abaixo temos um exemplo de um arquivo assim, onde a primeira
linha é o título de cada coluna e as demais são os valores de cada
coluna separados por vírgulas.
 exemplo de arquivo de log:
-------------------------------------
Day;Time;Event;Card;
2016-09-27;19:31:52;Normal;208772;
2016-09-27;18:12:02;Normal;155759;
2016-09-26;17:12:02;Normal;155759;
2016-09-26;16:11:02;Denied;188758;
2016-09-25;19:12:02;Normal;155759;
-------------------------------------
 Neste exemplo, de um log de controle de acesso de determinado mês,
temos:
 um campo de data (ano-mês-dia) e hora (hora-minuto-segundo),
 um campo com o tipo de evento de acesso (Normal ou Denied),
 e o número de identificação do usuário.
 Nesse exemplo temos 5 eventos, em 3 dias (25, 26 e 27 de
setembro), com 3 usuários diferentes.

 Abaixo temos uma String a ser usada para testes, sem a linha de
cabeçalho mas com 5 linhas: -}
logSetembro = "2016-09-27;19:31:52;Normal;208772;\n2016-09-27;18:12:02;Normal;155759;\n2016-09-26;17:12:02;Normal;155759;\n2016-09-26;16:11:02;Denied;188758;\n2016-09-25;19:12:02;Normal;155759;"

getLogByLog :: String -> [String]
getLogByLog [] = []
getLogByLog s | findChar s '\n' 0 == -1 = [s]
              | otherwise = [take (findChar s '\n' 0) s] ++ getLogByLog (takeRest s ((findChar s '\n' 0)+1))

organizeData :: String -> [String]
organizeData [] = []
organizeData s | findChar s ';' 0 == -1 = [s]
               | otherwise = [take ((findChar s ';' 0)) s] ++ organizeData (takeRest s ((findChar s ';' 0)+1))

listOfLogs :: String -> [[String]]
listOfLogs [] = []
listOfLogs s = map organizeData (getLogByLog s)

getAccess :: [String] -> String
getAccess (dia:hora:access:tail) = access

getAccessByDay :: [String] -> Int
getAccessByDay (dia:hora:access:tail) = getNumberDay dia

getNumberDay :: String -> Int
getNumberDay s = strToInt(takeRest s 8)

getUser :: [String] -> Int
getUser (dia:hora:access:user:tail) = strToInt(user)

getAllAccessByDay :: [[String]] -> [Int]
getAllAccessByDay s = map getAccessByDay s

getAllAccess :: [[String]] -> [String]
getAllAccess s = map getAccess s

getAllUsers :: [[String]] -> [Int]
getAllUsers s = map getUser s

getAccessMonth :: [Int] -> Int -> [(Int, Int)]
getAccessMonth list 1 | length (filter (==1) list) > 0 = [(1, length (filter (==1) list))]
                      | otherwise = []
getAccessMonth list n | length (filter (==n) list) > 0 = (n, length (filter (==n) list)):getAccessMonth list (n-1)
                      | otherwise = getAccessMonth list (n-1)

getRequest :: Int -> [Int] -> (Int, Int)
getRequest n list = (n, length (filter (==n) list))

getUserMonth :: [Int] -> [Int] -> [(Int, Int)]
getUserMonth [] list = []
getUserMonth (a:at) list = (getRequest a list):(getUserMonth at list)

{- Considerando que o arquivo está em uma String, e que os dados são
de um mesmo mês, você deve escrever funções que dêem as seguintes
informações: -}
-- 1) (2.5) Quantos acessos foram permitidos e quantos foram negados no período? (isto é, na string toda).
-- o resultado é uma tupla, onde o primeiro valor é o total de acessos permitidos (Normal) e o segundo o de Negados (Denied)
-- exemplo:
-- tiposDeAcesso logSetembro -----> (4,1)
-- tiposDeAcesso :: String -> (Int, Int) 

tiposDeAcesso :: String -> (Int,Int)
tiposDeAcesso s = (length (filter (=="Normal") (getAllAccess (listOfLogs s))),length (filter (=="Denied") (getAllAccess (listOfLogs s))))

-- 2) (2.5) Quantas tentativas de acessos ocorreram em cada dia? (considere que o arquivo já está ordenado por data e hora)cada dia com acesso deve aparecer em uma lista de tuplas, o primeiro valor da tupla é o dia do mês e o segundo o número de acessos.
-- exemplo:
-- acessosPorDia logSetembro -----> [(27,2),(26,2),(25,1)] -- 2 acessos dia 27, 2 dia 26 e 1 dia 25
-- acessosPorDia :: String -> [(Int, Int)]

acessosPorDia :: String -> [(Int, Int)]
acessosPorDia s = getAccessMonth (getAllAccessByDay (listOfLogs s)) (head (getAllAccessByDay (listOfLogs s)))

-- 3) (2.5) Quantos acessos cada usuário realizou no período?
-- exemplo:
-- acessosPorUsuario logSetembro -----> [(208772,1),(155759,3),(188758,1)] -- 3 acessos do usuário 155759 e 1 acesso dos outros
-- acessosPorUsuario :: String -> [(Int, Int)]

acessosPorUsuario :: String -> [(Int, Int)]
acessosPorUsuario s = uniq (getUserMonth (getAllUsers (listOfLogs s)) (getAllUsers (listOfLogs s)))

-- 4) (2.5) escreva a função converte, que transforma os dados armazenados na String para o seguinte tipo de dados:
type Dia = String
type Hora = String
type Usuario = String
data LogEntry = Permitido Dia Hora Usuario
 | Negado Dia Hora Usuario
 deriving Show
-- exemplo:
-- converte logSetembro -----> [Permitido "2016-09-27" "19:31:52" "208772",Permitido "2016-09-27" "18:12:02" "155759",Permitido "2016-09-26" "17:12:02" "155759",Negado "2016-09-26" "16:11:02" "188758",Permitido "2016-09-25" "19:12:02" "155759"]
-- converte :: String -> [LogEntry]

converte :: String -> [LogEntry]
converte s = map reduceLog (listOfLogs s)

reduceLog :: [String] -> LogEntry
reduceLog [] = error "string vazia"
reduceLog (dia:hora:access:user:tail) | access == "Normal" = (Permitido dia hora user)
                                      | access == "Denied" = (Negado dia hora user)

-- função auxiliar para as questões
strToInt :: String -> Int
strToInt str = read str

-- Funções auxiliares
uniq :: [(Int,Int)] -> [(Int,Int)]
uniq [] = [] 
uniq (a:at) = a:uniq[x | x <- at, x /= a]

takeRest :: String -> Int -> String
takeRest [] _ = []
takeRest s n = reverse (take ((length s)-n) (reverse s))

findChar :: String -> Char -> Int -> Int
findChar [] _ _ = -1
findChar (a:at) c n | a == c = n
                    | otherwise = findChar at c (n+1)