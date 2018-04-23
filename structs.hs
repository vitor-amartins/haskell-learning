data Stack t = Stk [t] 
data Queue t = Que [t]
    deriving (Show)

instance StructFunc Stack where
    pop (Stk []) = (Stk [])
    pop (Stk (a:at)) = (Stk at)
    push n (Stk t) = (Stk (n:t))
    top (Stk []) = error "Stack is empty!"
    top (Stk (a:at)) = a
    (+++) (Stk s1) (Stk s2) = (Stk (s1++s2))

instance StructFunc Queue where
    pop (Que []) = (Que [])
    pop (Que [a]) = (Que [])
    pop (Que (a:at)) = (Que [a])+++(pop (Que at))
    push n (Que t) = (Que (t++[n]))
    top (Que []) = error "Queue is empty!"
    top (Que [a]) = a
    top (Que (a:at)) = top (Que at)
    (+++) (Que q1) (Que q2) = (Que (q1++q2))

class StructFunc q where
    pop :: q t -> q t
    push :: t -> q t -> q t
    top :: q t -> t
    (+++) :: q t -> q t -> q t

data BinaryTree t = Nil | Node t (BinaryTree t) (BinaryTree t)
    deriving(Show, Eq, Ord)

tree1 = Node 10 (Node 3 Nil Nil) (Node 12 Nil Nil)

insert :: (Ord t) => t -> BinaryTree t -> BinaryTree t
insert v (Nil) = (Node v Nil Nil)
insert v (Node node t1 t2) | v <= node = (Node node (insert v t1) t2)
                           | otherwise = (Node node t1 (insert v t2))

contain :: (Ord t) => t -> BinaryTree t -> Bool
contain v (Nil) = False
contain v (Node node t1 t2) | v == node = True
                            | v < node = contain v t1
                            | otherwise = contain v t2

remove :: (Ord t) => t -> BinaryTree t -> BinaryTree t
remove v (Nil) = Nil
remove v (Node node t1 t2) | v < node = (Node node (remove v t1) t2)
                           | v > node = (Node node t1 (remove v t2))
                           | v == node && t1 == Nil && t2 == Nil = (Nil)

-- instance OprBinTree BinaryTree where
    -- insert n (Empty) = (Node n (Empty) (Empty)) 
    -- insert value (Node n (t1) (t2)) | value <= n = (Node n (insert value t1) (t2))
    --                                 | otherwise = (Node n (t1) (insert value t2)) 

-- instance Show BinaryTree where
--     show (Nil) = "Nil"
--     show (Node t (t1) (t2)) = "(" ++ t1 ++ " " ++ show (t1) ++ ", " ++ show (t2) ++ ")"

-- class OprBinTree q where
--     insert :: t -> q t -> q t
