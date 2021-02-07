module Teste19_20 where

--EXERCÍCIO 1
--a
intersect :: Eq a => [a] -> [a] -> [a]
intersect [] _ = []
intersect l [] = l
intersect (x:xs) l | elem x l = x : intersect xs l
                   | otherwise = intersect xs l

--b
tails :: [a] -> [[a]]
tails [] = [[]]
tails l = l : (tails (tail l))

--EXERCÍCIO 2
type ConjInt = [Intervalo]
type Intervalo = (Int,Int)

--a
elems :: ConjInt -> [Int]
elems [] = []
elems [(x,y)] = [x..y] 
elems ((x,y):xs) = [x..y] ++ elems xs

--b
{-geraconj :: [Int] -> ConjInt
geraconj h = auxb h

auxb :: [Int]-> ConjInt -> ConjInt
auxb [] a = a
auxb (h:t) [] = auxb t [(h,h)]
auxb (h:t) l  | h-1 == f = auxb t ((init l)++ [(i,h)])
              | otherwise = auxb t (l ++ [(h,h)])
        where (i,f) = last l


-}
geraconj :: [Int] -> ConjInt
geraconj [] = []
geraconj l = (head (geraaux l), last (geraaux l)) : geraconj (take (length (geraaux l)) l)


geraaux :: [Int] -> [Int]
geraaux [] = [] 
geraaux [x] = [x]
geraaux (h:h1:t)
            | h +1 == h1 = h : geraaux (h1:t)
            | otherwise = [h] 

--EXERCÍCIO 3
data Contacto = Casa Integer
                | Trab Integer
                | Tlm Integer
                | Email String
        deriving (Show)
type Nome = String
type Agenda = [(Nome, [Contacto])]

--a
acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail x y [] = [(x,[Email y])]
acrescEmail x y ((a,n):xs) | x == a = [(a, (Email y):n)]++xs
                           | otherwise = (a,n): acrescEmail x y xs
--b
verEmails :: Nome -> Agenda -> Maybe [String]
verEmails n [] = Nothing
verEmails n ((x,y):xs) | n == x = Just (filtramails y)
                       | otherwise = verEmails n xs

filtramails :: [Contacto] -> [String]
filtramails [] = []
filtramails ((Email m):ms) = ( m) : filtramails ms
filtramails (m:ms) = filtramails ms

--c
consulta :: [Contacto] -> ([Integer],[String])
consulta [] = ([],[])
consulta (Email x:xs) = (n,x:m)
  where (n,m) = consulta xs 
consulta (Casa x:xs) = (x:n,m)
  where (n,m) = consulta xs 
consulta (Tlm x:xs) = (x:n,m)
  where (n,m) = consulta xs
consulta (Trab x:xs) = (x:n,m)
  where (n,m) = consulta xs  

--d
consultaIO :: Agenda -> IO ()
consultaIO x = do nome <- getLine
                  putStrLn $ show (snd((filter (\y -> fst(y)==nome) x)!!0))

--EXERCÍCIO 4

data RTree a = R a [RTree a]
   deriving (Show, Eq)

ex :: RTree Int 
ex=(R 1 [R 2 [], R 3 [R 4 [R 5 [], R 6 []]],
         R 7 []])

--a MAL
paths :: RTree a -> [[a]]
paths (R n t) = aux [R n t]

aux :: [RTree a] -> [[a]] 
aux [R n []] = [[n]]
aux [R n t] = [n] : (aux t)

--b
unpaths :: Eq a => [[a]] -> RTree a
unpaths [] = error "Impossivel"
unpaths [[x]] = (R x [])
unpaths ((x:y):xs) = (R x [unpaths y])

aux4 (x:xs) = (R x [aux4 xs])