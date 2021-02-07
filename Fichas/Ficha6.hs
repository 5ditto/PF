module FICHA6 where

--EXERCÍCIO 1
data BTree a = Empty 
             | Node a (BTree a) (BTree a)
           deriving Show

arv = (Node 5 (Node 7 (Node 3 Empty Empty)
                      (Node 2 (Node 10 Empty Empty) Empty)
              )
              (Node 1 (Node 12 Empty Empty)
                      (Node 4 Empty (Node 8 Empty Empty))
              )
      )

--c
folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node x Empty Empty) = 1
folhas (Node x y z) = folhas y + folhas z

--d
prune :: Int -> BTree a -> BTree a
prune _ Empty = Empty
prune 0 _  = Empty
prune n (Node x e d)| n>0 = Node x (prune (n-1) e) (prune (n-1) d)

--e
path :: [Bool] -> BTree a -> [a]
path [] (Node n e d) = [n]
path _ Empty = []
path (x:xs) (Node n e d) | x == False = n : path xs e 
                         | otherwise = n : path xs d

--f
mirror :: BTree a -> BTree a 
mirror Empty = Empty
mirror (Node n e d) = (Node n (mirror d) (mirror e))

--EXERCÍCIO 2
--a
minimo :: Ord a => BTree a -> a 
minimo (Node x Empty d) = x
minimo (Node _ e _) = minimo e 


--b
semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node x Empty d) = d
semMinimo (Node x e d) = Node x (semMinimo e) d

--c
minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin (Node x Empty d) = (x,d)
minSmin (Node x e d) = let (m,e') = minSmin e
                       in (m,Node x e' d)

--d
remove :: Ord a => a -> BTree a -> BTree a
remove _ Empty = Empty
remove x (Node y e d) | x<y = Node y (remove x e) d
                      | x>y = Node y e (remove x d)
                      | x==y = let (z,d') = minSmin2 d
                               in  Node z e d'
minimo :: Ord a => BTree a -> a



arvpro =  Node 7 (Node 5 Empty ( Node 6 Empty Empty))
                 (Node 10 (Node 8 Empty (Node 9 Empty Empty))(Node 20 Empty Empty))

--EXERCÍCIO 3
type Aluno = (Numero, Nome, Regime, Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL 
    deriving Show
data Classificacao = Aprov Int
                   | Rep 
                   | Faltou 
    deriving Show
type Turma = BTree Aluno 