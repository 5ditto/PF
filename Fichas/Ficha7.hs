module Ficha7 where

--Exercício 1
data ExpInt = Const Int
            | Simetrico ExpInt
            | Mais ExpInt ExpInt
            | Menos ExpInt ExpInt
            | Mult ExpInt ExpInt
    deriving (Show)

ex1 :: ExpInt
ex1 = Mais (Const 3) (Mult (Const 7) (Const 5))

ex2 :: ExpInt
ex2 = Menos (Mais (Const 3) (Mult (Const 7) (Const 5))) (Mais (Const 10) (Const 20))

--a
calcula :: ExpInt -> Int
calcula (Const x) = x
calcula (Simetrico e) = calcula e
calcula (Mais e1 e2) = (calcula e1) + (calcula e2)
calcula (Menos e1 e2) = (calcula e1) - (calcula e2)
calcula (Mult e1 e2) = (calcula e1) * (calcula e2)

--b
infixa :: ExpInt -> String
infixa (Const x) = show x
infixa (Simetrico e) = "~" ++ (infixa e) 
infixa (Mais e1 e2) = "(" ++ (infixa e1) ++ "+" ++ (infixa e2) ++ ")"
infixa (Menos e1 e2) = "(" ++ (infixa e1) ++ "-" ++ (infixa e2) ++ ")"
infixa (Mult e1 e2) = "(" ++ (infixa e1) ++ "*" ++ (infixa e2) ++ ")"

--c
posfixa :: ExpInt -> String
posfixa (Const x) = show x
posfixa (Simetrico e ) = (posfixa e) ++ " ~"
posfixa (Mais e1 e2) = (posfixa e1) ++ " " ++ (posfixa e2) ++ " +"
posfixa (Menos e1 e2) = (posfixa e1) ++ " " ++ (posfixa e2) ++ " -"
posfixa (Mult e1 e2) = (posfixa e1) ++ " " ++ (posfixa e2) ++ " *"

--Exercício 2
data RTree a = R a [RTree a]
   deriving (Show)

exe1 = R 5 [ R 4 [ R 3 [ R 17 []], R 2 [], R 7 []],
             R 10 [],
             R 1 [ R 8 [ R 0 [], R 20 [], R 15 [], R 39 [] ],
                   R 12 []]
           ]

--a
soma :: Num a => RTree a -> a
soma (R x l) = x + sum ( map soma l)

--c
prune :: Int -> RTree a -> RTree a
prune 0 _ = error "n tem de ser maior do que 0"
prune n (R x l) | n == 1 = (R x [])
                | n >  1 = (R x (map (prune (n-1)) l))

--Exercício 3

data LTree a  = Tip a 
              | Fork (LTree a) (LTree a)
   deriving (Show)

exe2 = Fork (Fork (Tip 7) (Tip 8)) (Fork (Tip 4) (Fork (Tip 1) (Tip 6)))

--a 
ltSum :: Num a => LTree a -> a
ltSum (Tip x) = x
ltSum (Fork y x) = ltSum y + ltSum x

--b
listaLT :: LTree a -> [a]
listaLT (Tip a) = [a]
listaLT (Fork x y) = listaLT x ++ listaLT y

--c 
ltHeight :: LTree a -> Int
ltHeight (Tip a) = 1
ltHeight (Fork x y) = 1 + max (ltHeight x) (ltHeight y)

--Exercício 4
data BTree a = Empty 
             | Node a (BTree a) (BTree a)
    deriving (Show)

data FTree a b = Leaf b 
               | No a (FTree a b) (FTree a b)
    deriving (Show)

exe = No 1 (No 4 (No 5 (Leaf 'A') (Leaf 'B')) (Leaf 'C')) 
           (No 7 (Leaf 'D') (Leaf 'E'))
--a
splitFTree :: FTree a b -> (BTree a, LTree b)
splitFTree (Leaf x) = (Empty, Tip x)
splitFTree (No n x y) = let (eb,el) = splitFTree x
                            (db,dl) = splitFTree y
                        in (Node n eb db, Fork el dl)

--b
joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
