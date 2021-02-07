module Ficha where

-----Exercício 5 -----
data Movimento = Credito Float | Debito Float
     deriving Show

data Data = D Int Int Int 
     deriving Show

data Extrato = Ext Float [(Data, String, Movimento)]
     deriving Show

ex1 :: Extrato
ex1 = Ext 100 [(D 2020 1 30 , "compra", Debito 15), (D 2020 2 14, "salario", Credito 850)]

---A
extValor :: Extrato -> Float -> [Movimento]
extValor (Ext x e ) n = aux n e 

aux :: Float -> [(Data, String, Movimento)] -> [Movimento]
aux x [] = []
aux x ((_, _, Debito m):t) | x >= m = (Debito m ): aux x t
                          | otherwise = aux x t
aux x ((_, _, Credito m):t) | x >= m = (Credito m ): aux x t
                          | otherwise = aux x t

---B
filtro :: Extrato -> [String] -> [(Data, Movimento)]
filtro (Ext x e) nomes = aux1 e nomes

aux1 :: [(Data, String, Movimento)] -> [String] -> [(Data, Movimento)]
aux1 [] _ = []
aux1 ((d, n, c):t) nomes | elem n nomes = (d,c) : aux1 t nomes
                         | otherwise = aux1 t nomes

---C
--[Extarto] -> [Credito a+Credito b, Debito a+ debito b]
creDeb :: Extrato -> (Float,Float) ---soma o credito e o debito dos movimentos 
creDeb (Ext x e) = aux2 e

aux2 :: [(Data, String, Movimento)] -> (Float,Float)
aux2 [] = (0,0)
aux2 ((_,_,Credito m):t) = (fst (aux2 t), m+ snd (aux2 t))
aux2 ((_,_,Debito m):t)= (m +x , y)
    where (x,y) = aux2 t

---D

saldo :: Extrato -> Float
saldo (Ext x e) = let (c,d) = creDeb (Ext x e)
                  in x + d - c

