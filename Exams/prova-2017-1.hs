-- Primeira Prova de Paradigmas de Linguagens Computacionais 
-- 1/2017 - 01/05/2017
--
-- Nome: Vitor Matheus
--
{- 1) (2.5) Escreva uma função que verifica se uma lista já está ordenada, 
   do menor para o maior elemento..
   exemplo: isSorted [1,6,8,9,9] ------> True
            isSorted [1,6,8,7,9] ------> False
   Dica: verifique se sua resposta funciona para listas de tamanho ímpar.
-}
-- isSorted :: Ord t => [t] -> Bool

isSorted :: Ord t => [t] -> Bool
isSorted [] = True
isSorted (a:at) | (foldr min a at) == a = isSorted at
                | otherwise = False

{- 2) (2.5) O método de ordenação bubble-sort funciona da seguinte forma: 
   cada elemento da lista de entrada é comparado com o seguinte, 
   e se eles não estiverem em ordem (do menor para o maior) sua posição na lista resultante é trocada,
   e a comparação continua com a nova ordem.Esse processo é repetido até que a lista esteja ordenada 
   (nenhuma troca seja mais necessária).
   exemplo, passo a passo: 
       bSort [4,8,3,6,1,8] ----> compara 4 e 8, 8 e 3 (troca, pois 8 > 3), 8 e 6(troca novamente), 8 e 1 (troca novamente) e 8 e 8  
                                   ----> [4,3,6,1,8,8]
       repetindo o processo, temos  ---> [3,4,1,6,8,8] ---> [3,1,4,6,8,8]  ---> [1,3,4,6,8,8]
Implemente a função bSort.
Dica 1: use funções auxiliares, que façam parte do processo;
Dica 2: verifique que sua solução funciona para listas de tamanho ímpar.
-}
-- bSort :: Ord t => [t] -> [t]

bSort :: Ord t => [t] -> [t]
bSort [] = []
bSort list | list == comp list = list
           | otherwise = bSort (comp list)

comp :: Ord t => [t] -> [t]
comp [a] = [a]
comp (a1:a2:at) | a1 <= a2 = a1:comp(a2:at)
                | otherwise = a2:comp(a1:at)

{- 3) (2.5) explique como funciona e informe qual o resultado da execução das 
   seguintes expressões. Caso estejam erradas explique por que.
a) map (\x -> x + x) [3,5,7,9]
-- O map recebe uma função e uma lista, gerando uma nova lista onde cada elemento é o resultado da função aplicada ao elemento de mesmo índice da lista passada como parâmetro.
-- Neste caso o map recebe uma função que retorna o dobro da entrada. Então a lista resultante será uma lista com cada elemento sendo o dobro dos elementos da lista de entrada.
-- map (\x -> x + x) [3,5,7,9] = [6,10,14,18]
b) filter (\x -> x < 7) [5,7,9,11]
-- O filter recebe uma função e uma lista, gerando uma nova lista com os elementos que satisfazem as condições da função passada.
-- Ou seja a função retorna um Boolean, e cada elemento da lista só será incluído na nova lista se a função retornar True.
-- filter (\x -> x < 7) [5,7,9,11] = [5]
c) foldr1 (*) [-2,0,2,4]
-- O foldr1 recebe uma função e uma lista, e ele vai aplicar esta função aos elementos da lista.
-- Inicialmente aos dois primeiros, e depois o resultado disso ao próximo elemento, fazendo isso até o fim da lista, começando pela direita.
-- 1º: -2 * 0 = 0
-- 2º:  0 * 2 = 0
-- 3º:  0 * 4 = 0
-- foldr1 (*) [-2,0,2,4] = 0
d) foldr (+) 20 [-2,0,2,4]
-- O foldr recebe uma função, um elemento e uma lista, e ele vai aplicar esta função aos elementos da lista.
-- Inicialmente ao elemento passado e ao primeiro elemento da lista, e depois o resultado disso ao próximo elemento, fazendo isso até o fim da lista, começando pela direita.
-- 1º: 20 + -2 = 18
-- 2º: 18 +  0 = 18
-- 3º: 18 +  2 = 20
-- 4º: 20 +  4 = 24
-- foldr (+) 20 [-2,0,2,4] = 24
e) (map (+2) . filter (<7)) [5,7,9,11]
-- map :: (a -> b) -> ([a] -> [b])
-- map (+2) :: [a] -> [b]
-- filter :: (g -> Bool) -> ([g] -> [g])
-- filter (<7) :: [g] -> [g]
-- (.) :: (d -> e) -> (f -> d) -> (f -> e)
-- d :: [a]
-- e :: [b]
-- f :: [g]
-- d :: [g]
-- g == a
-- (map (+2) . filter (<7)) :: [g] -> [b]
-- Podemos ver que a composição das funções funciona se 'g' for do mesmo tipo que 'a'
-- Primeiro ele vai aplicar o filter à lista de entrada, retornando [5]
-- Esse resultado será aplicado na função map (+2), retornando [7].
-}

{- 4) (2.5) Dada o tipo de dados Tree t, abaixo, que reresenta uma árvore binária 
com informações (valores) em seus nós, faça uma função isSortedTree que informa se uma árvore está ordenada, ou seja, os valores em nós ou folhas na sub-àrvore à esquerda são sempre menores ou iguais ao valor do nó, e os da sub-árvore à direita sempre maiores ou iguais.
-}
data Tree t = Node t (Tree t) (Tree t) 
            | Leaf t
testeOrdenado :: Tree Int
testeOrdenado = Node 10 (Node 5 (Leaf 3) (Leaf 6)) (Node 15 (Leaf 14) (Leaf 17))
testeNaoOrdenado :: Tree Int
testeNaoOrdenado = Node 10 (Node 5 (Leaf 3) (Leaf 6)) (Node 15 (Leaf 16) (Leaf 17))
-- isSortedTree testeOrdenado ----> True
-- isSortedTree testeNaoOrdenado ----> False
-- isSortedTree :: Ord t => Tree t -> Bool

isSortedTree :: Ord t => Tree t -> Bool
isSortedTree (Leaf s) = True
isSortedTree (Node s t1 t2) = checkLeft t1 s && checkRight t2 s && isSortedTree t1 && isSortedTree t2

checkLeft :: Ord t => Tree t -> t -> Bool
checkLeft (Leaf s) n = s <= n
checkLeft (Node s t1 t2) n = s <= n && checkLeft t1 n && checkLeft t2 n

checkRight :: Ord t => Tree t -> t -> Bool
checkRight (Leaf s) n = s >= n
checkRight (Node s t1 t2) n = s >= n && checkRight t1 n && checkRight t2 n