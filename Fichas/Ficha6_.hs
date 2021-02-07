module Ficha6 where 

--Exercício 1
data BTree a = Empty | Node a (BTree a) (BTree a)
    deriving Show

arv = (Node 5 (Node 7 (Node 3 Empty Empty)
                      (Node 2 (Node 10 Empty Empty) Empty)
              )
              (Node 1 (Node 12 Empty Empty)
                      (Node 4 Empty (Node 8 Empty Empty))
              )
      )

--a
altura :: BTree a -> Int 
altura Empty = 0
altura (Node n e d) = 1 + max (altura d)  (altura e)

--b
contaNodos :: BTree a -> Int
contaNodos Empty = 0
contaNodos (Node n e d) = 1 + altura e + altura d

--c
folhas :: BTree a -> Int
folhas (Node n Empty Empty) = 1 
folhas Empty = 0 
folhas (Node n d e) = (folhas e) + (folhas d)

--d
prune :: Int -> BTree a -> BTree a
prune 0 _ = Empty
prune _ Empty = Empty 
prune n (Node x e d) |n>0 = Node x (prune (n-1) e ) (prune (n-1) d)

--e
path :: [Bool] -> BTree a -> [a]
path _ Empty = []
path [] (Node n e d) = [n]
path (x:xs) (Node n e d) | x == False = n : path xs e
                         | otherwise = n : path xs d

--f
mirror :: BTree a -> BTree a 
mirror Empty = Empty
mirror (Node x e d) = (Node x (mirror d) (mirror e))

--g
zipWithBT ::(a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT _ _ Empty = Empty
zipWithBT _ Empty _ = Empty
zipWithBT f (Node n e d) (Node x e1 d1) = Node (f n x) (zipWithBT f e e1) (zipWithBT f d d1)

--h
unzipBT :: BTree (a,b,c) -> (BTree a, BTree b, BTree c)
unzipBT Empty = (Empty,Empty,Empty)
unzipBT (Node (x,y,z) e d)= (Node x unzipe1 unzipd1, Node y unzipe2 unzipd2, Node z unzipe3 unzipd3)
   where (unzipe1,unzipe2,unzipe3) = unzipBT e
         (unzipd1,unzipd2,unzipd3) = unzipBT d

--Exercicio 2
arvpro =  Node 7 (Node 5 Empty ( Node 6 Empty Empty))
                 (Node 10 (Node 8 Empty (Node 9 Empty Empty))(Node 20 Empty Empty))

--a
minimo :: Ord a => BTree a -> a
minimo Empty = error "impossivel"
minimo (Node x Empty d) = x
minimo (Node x e d) = minimo e

--b
semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node x Empty d) = d
semMinimo (Node x e d) = (Node x (semMinimo e) d )

--c
minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin m = (minimo m, semMinimo m)

--d
remove :: Ord a => a -> BTree a -> BTree a
remove n Empty = Empty
remove x (Node n e d) | x > n = Node n e (remove x d)
                      | x < n = Node n (remove x e) d
                      | x == n = let (z,d') = minSmin d 
                                 in (Node z e d')
--Exercício 3
type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data  Regime = ORD | TE | MEL 
        deriving (Show,Eq)
data Classificacao = Aprov Int
                    | Rep
                    | Faltou 
        deriving (Show,Eq)
type Turma = BTree Aluno -- árvore binária de procura (ordenada por número)

turma1 :: Turma
turma1 = (Node (15,"Luís",ORD,Aprov 14) (Node (12,"Joana",MEL,Faltou) (Node (7,"Diogo",TE,Rep) Empty
                                                                                               Empty) 
                                                                      (Node (14,"Lara",ORD,Aprov 19) Empty
                                                                                                     Empty))
                                        (Node (20,"Pedro",TE,Aprov 10) Empty
                                                                       (Node (25,"Sofia",ORD,Aprov 20) (Node (23,"Rita",ORD,Aprov 17) Empty
                                                                                                                                      Empty)
                                                                                                       (Node (28,"Vasco",MEL,Rep) Empty
                                                                                                                                  Empty))))

--a
inscNum :: Numero -> Turma -> Bool
inscNum _ Empty = False
inscNum n (Node (num,_,_,_) e d) | n == num = True
                                 | n > num = inscNum n d
                                 | n < num = inscNum n e

--b
inscNome :: Nome -> Turma -> Bool   
inscNome _ Empty = False
inscNome n (Node (_,nome,_,_) e d) | n == nome = True
                                   | otherwise = (inscNome n e) || (inscNome n d)

--c
trabEst :: Turma -> [(Numero,Nome)] 
trabEst Empty = []
trabEst (Node (num,nome,r,_) e d) | TE == r  = [(num,nome)] ++ trabEst e ++ trabEst d
                                  | otherwise = trabEst e ++ trabEst d

--d
nota :: Numero -> Turma -> Maybe Classificacao
nota n m@(Node (num,_,_,clas) e d) | inscNum n m == False = Nothing
                                   | n == num = Just clas
                                   | n < num = nota n e
                                   | n > num = nota n d

--e
percFaltas :: Turma -> Float
percFaltas Empty = 0
percFaltas turma = (sumfaltas turma / numalunos turma) 
   where sumfaltas :: Turma -> Float
         sumfaltas Empty = 0
         sumfaltas (Node (_,_,_,clas) e d) | clas == Faltou = 1 + sumfaltas e + sumfaltas d
                                           | otherwise = sumfaltas e + sumfaltas d
         numalunos :: Turma -> Float
         numalunos Empty = 0
         numalunos (Node (_,_,_,_) e d) = 1 + numalunos e + numalunos d

--f
mediaAprov :: Turma -> Float
mediaAprov Empty = 0
mediaAprov turma = (somanotas turma) / (numnotas turma)
    where somanotas :: Turma -> Float
          somanotas Empty = 0
          somanotas (Node (_,_,_, Aprov n) e d) = fromIntegral n + somanotas e + somanotas d
          somanotas (Node _ e d) = somanotas e + somanotas d

          numnotas :: Turma -> Float
          numnotas  Empty = 0
          numnotas (Node (_,_,_, Aprov n) e d) = 1 + numnotas e + numnotas d
          numnotas (Node _ e d) = numnotas e + numnotas d

--g
aprovAv :: Turma -> Float
aprovAv Empty = 0
aprovAv turma = a/r
    where (a,r) = aux turma
          aux :: Turma -> (Float,Float)
          aux Empty = (0,0)
          aux (Node (_,_,_,clas) e d) = case clas of Aprov _ -> (x+1,y); Rep -> (x,y+1); otherwise -> (x,y)
                where (x,y)=(c+i,h+f)
                      (c,h) = aux e
                      (i,f) = aux d