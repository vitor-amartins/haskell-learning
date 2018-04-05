{- Aula 09 - 05/04 -}

data Instrucao = PUSH Int | POP | ADD | SUB | DUP
type Pilha = [Int]

a :: Pilha
a = [1,2]

evalI :: Instrucao -> Pilha -> Pilha
evalI (PUSH n) stack = n:stack
evalI (POP) (a:at) = at
evalI (ADD) (a:b:at) = (a+b):at
evalI (SUB) (a:b:at) = (a-b):at
evalI (DUP) (a:at) = [a, a]++at

evalProg :: [Instrucao] -> Pilha
evalProg ins = evalProgSub ins []

evalProgSub :: [Instrucao] -> Pilha -> Pilha
evalProgSub [] stack = stack
evalProgSub (a:at) stack = evalProgSub at (evalI a stack)