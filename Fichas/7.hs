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
calcula (Const n) = n
calcula (Simetrico e) = calcula e 
calcula (Mais x y) = (calcula x) + (calcula y)
calcula (Menos x y) = (calcula x) - (calcula y)
calcula (Mult x y) = (calcula x) * (calcula y)

--b
infixa :: ExpInt -> String
infixa (Const x) = show x
infixa (Simetrico x) = "~" ++ (infixa x)
infixa (Mais x y) = "(" ++ (infixa x) ++ "+" ++ (infixa y) ++ ")"
infixa (Menos x y) = "(" ++ (infixa x) ++ "-" ++ (infixa y) ++ ")"
infixa (Mult x y) = "(" ++ (infixa x) ++ "*" ++ (infixa y) ++ ")"   

--c
posfixa :: ExpInt -> String
posfixa (Const x) = show x
posfixa (Simetrico x) = (posfixa x) ++ " ~"
posfixa (Mais x y) = (posfixa x) ++ " " ++ (posfixa y) ++ " +"
posfixa (Menos x y) = (posfixa x) ++ " " ++ (posfixa y) ++ " -"
posfixa (Mult x y) = (posfixa x) ++ " " ++ (posfixa y) ++ " *"

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
soma (R x l) = x + sum (map soma l)

--b
altura :: RTree a -> Int
altura (R x []) = 1
altura (R x xs) = 1 + maximum (map altura xs)

--c
prune :: Int -> RTree a -> RTree a
prune 0 _ = error "IMPOSSÍVEL"
prune n (R x l) | n == 1 = (R x [])
                | n > 1 = (R x (map (prune (n-1)) l) )

--d
mirror :: RTree a -> RTree a 
mirror (R x []) = (R x [])
mirror (R x l) = (R x (map mirror (reverse l)))

--e
postorder :: RTree a -> [a]
postorder (R x []) = [x]
postorder (R x l) = (concat (map postorder l)) ++ [x]

--Exercício 3

data BTree a = Empty | Node a (BTree a) (BTree a)
   deriving (Show)
data LTree a = Tip a | Fork (LTree a) (LTree a)
  deriving (Show)

exe2 = Fork (Fork (Tip 7) (Tip 8)) (Fork (Tip 4) (Fork (Tip 1) (Tip 6)))

--a
ltSum :: Num a => LTree a -> a
ltSum (Tip a) = a
ltSum (Fork e d) = (ltSum e) + (ltSum d) 

--b
listaLT :: LTree a -> [a]
listaLT (Tip a) = [a]
listaLT (Fork e d) =  (listaLT e) ++ (listaLT d)

--c
ltHeight :: LTree a -> Int
ltHeight (Tip a) = 1
ltHeight (Fork e d) = 1 + max (ltHeight e) (ltHeight d)

--Exercício 4
data FTree a b = Leaf b | No a (FTree a b) (FTree a b)
   deriving (Show)

ftree1 = No 8 (No 1 (Leaf 5)
                    (No 2 (Leaf 6)
                          (Leaf 4)))
              (No 9 (No 10 (Leaf 3)
                           (Leaf 7))
                    (Leaf 5))
--a
splitFTree :: FTree a b -> (BTree a, LTree b)
splitFTree (Leaf x) = (Empty, Tip x)
splitFTree (No a e d) = let (eb,el) = splitFTree e
                            (db,dl) = splitFTree d
                        in (Node a eb db,Fork el dl)

--b
joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
joinTrees Empty (Tip a) = Just (Leaf a)                        
joinTrees (Node n e d) (Fork e1 d1) = Just (No n aux aux1)
   where Just aux =  (joinTrees e e1)
         Just aux1 =  (joinTrees d d1)
joinTrees _ _ = Nothing