{- Aula 09 - 03/04 -}

data Temp = Quente | Frio
     deriving (Show, Eq)
data Estacao = Inverno | Verao | Outono | Primavera
     deriving (Show, Eq)

clima :: Estacao -> Temp
clima Inverno = Frio
clima _       = Quente

type Nome = String
type Idade = Int
data Pessoas = Pessoa Nome Idade

data Shape = Circle Double | Rectangle Double Double
     deriving(Show)

isRound :: Shape -> Bool
isRound (Circle _) = True
isRound (Rectangle _ _) = False

area :: Shape -> Double
area (Circle r) = pi*r*r
area (Rectangle l a) = l*a

data Expr = Lit Int | Add Expr Expr | Sub Expr Expr

eval :: Expr -> Int
eval (Lit n) = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Sub e1 e2) = (eval e1) - (eval e2)

showExpr :: Expr -> String
showExpr (Lit n) = show n
showExpr (Add e1 e2) = "(" ++ showExpr e1 ++ " + " ++ showExpr e2 ++ ")"
showExpr (Sub e1 e2) = "(" ++ showExpr e1 ++ " - " ++ showExpr e2 ++ ")"

data List t = Nil | Cons t (List t)

toList :: List t -> [t]
toList (Nil) = []
toList (Cons n li) = n:toList li

fromList :: [t] -> List t
fromList [] = (Nil)
fromList (a:at) = (Cons a (fromList at))

data Tree t = NilT | Node t (Tree t) (Tree t)

treet = Node 5 (Node 2 NilT NilT) NilT
treet2 = Node 11 (Node 45 NilT (Node 43 NilT NilT)) (Node 99 (Node 22 NilT NilT) (Node 21 NilT NilT))

depth :: Tree t -> Int
depth (NilT) = 0
depth (Node a t1 t2) = 1 + max (depth t1) (depth t2)

collapse :: Tree t -> [t]
collapse (NilT) = []
collapse (Node a t1 t2) = [a] ++ collapse t1 ++ collapse t2

mapTree :: (t -> u) -> Tree t -> Tree u
mapTree _ (NilT) = NilT
mapTree f (Node a t1 t2) = Node (f a) (mapTree f t1) (mapTree f t2)

