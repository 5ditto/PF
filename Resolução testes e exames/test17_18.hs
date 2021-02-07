module Teste17_18 where

import Data.List
--Exercício 1
insert'' :: Ord a => a -> [a] -> [a]
insert'' n [] = [n]
insert'' n (x:xs) | n <= x = n : (x:xs)
                  | n > x = x : insert'' n xs


--EXERCÍCIO 2
catMaybes :: [Maybe a] -> [a]
catMaybes [] = [] 
catMaybes (h:t) = case h of Just x -> x : catMaybes t 
                            otherwise -> catMaybes t

--EXERCÍCIO 3

data Exp a = Const a
           | Var String
           | Mais (Exp a) (Exp a)
           | Mult (Exp a) (Exp a)

ex =(Mais (Var "x") (Mult (Const 3) (Const 4)))

instance Show a => Show (Exp a)  where
    show = expa

expa :: Show a => Exp a -> String
expa (Const a) = show a
expa (Var a) = show a
expa (Mais a b) = "(" ++ expa a ++ " + " ++ expa b ++ ")"
expa (Mult a b) = "(" ++ expa a ++ " * " ++ expa b ++ ")"

--EXERCÍCIO 4
sortOn' :: Ord b => (a -> b) -> [a] -> [a]
sortOn' _ [] = []
sortOn' f (x:xs) = insert' x (sortOn f xs)
   where insert' a [] = [a]
         insert' a (x:xs) | f a > f x = x : insert' a xs
                          | otherwise = a:x:xs

--Exercício 5

--a 
amplitude :: [Int] -> Int
amplitude [] = 0
amplitude l = let (x,y) = (minimum l, maximum l)
              in y - x

--b
{-
parte :: [Int] -> ([Int],[Int])
parte m = aux (sort m)

aux :: [Int] -> ([Int],[Int])
aux [] = ([],[])
aux m = auxmenor h t (init (tail m))
    where h = head m
          t = last m

auxmenor :: Int -> Int -> [Int] -> ([Int],[Int])
auxmenor _ _ [] = []
auxmenor n m (x:xs) |(amplitude n x) <= (amplitude m x) = (n:x:ts,m:t)
                    |(amplitude n x) > (amplitude m x) = (x:ts,m:x:t)
        where (ts,t) = auxmenor n m xs
-}
--EXERCÍCIO 6
data Imagem = Quadrado Int
            | Mover (Int,Int) Imagem
            | Juntar [Imagem]

ex1 :: Imagem
ex1 = Mover (5,5) (Juntar [Mover (0,1) (Quadrado 5),
                         Quadrado 4,
                         Mover (4,3) (Quadrado 2)])

--a
conta :: Imagem -> Int
conta (Quadrado _) = 1
conta (Mover (_,_) x) = conta x
conta (Juntar l) = sum (map conta l)

--b
apaga :: Imagem -> IO Imagem
apaga im = do 
     let in