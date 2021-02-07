module Teste16_17 where

import Data.Maybe

--EXERCÍCIO 1

type MSet a = [(a,Int)]


ex :: MSet Char
ex = [('b',4),('a',2),('c',1)]
--{’b’,’a’,’c’,’b’,’b’,’a’,’b’}
--(a,b):(c,d)
-- a/=c e b, d >= 0

--a
cardMSet :: MSet a -> Int
cardMSet [] = 0
cardMSet ((x,y):xs) =  y + (cardMSet xs)

--b
moda :: MSet a -> [a]
moda [] = []
moda [(x,y)] = [x]
--moda l@((x,y):xs) = fst $ filter (==maximum (map (snd) l))


--c
converteMSet :: MSet a -> [a]
converteMSet [] = []
converteMSet ((x,y):xs) | y/=1 = x : converteMSet ((x,y-1):xs) 
                        | otherwise = x : converteMSet xs

--d
addNcopies :: Eq a => MSet a -> a -> Int -> MSet a 
addNcopies [] x n = [(x,n)]
addNcopies m@((a,b):t) x n | elem x (map (fst) m) = aux1 x n m
                           | otherwise = aux2 x n m   

aux1 :: Eq a => a -> Int -> MSet a -> MSet a
aux1 x n m@((a,b):t) | x == a = (a,b+n) : t
                     | otherwise = (a,b) : aux1 x n t

aux2 :: a -> Int -> MSet a -> MSet a
aux2 x n [] = [(x,n)]
aux2 x n m@((a,b):t) | n >= b = (x,n) : m
                     | n < b = (a,b) : aux2 x n t


--EXERCÍCIO 2
data SReais = AA Double Double | FF Double Double
            | AF Double Double | FA Double Double
            | Uniao SReais SReais

ex2 :: SReais
ex2 = Uniao (Uniao (AA 4.2 5.5) (AF 3.1 7.0)) (FF (-12.3) 30.0)

--a
instance Show SReais where
    show = intervalos

intervalos :: SReais ->  String
intervalos (AA x y) = "]" ++ show x ++ " , " ++ show y ++ "["
intervalos (AF x y) = "]" ++ show x ++ " , " ++ show y ++ "]"
intervalos (FA x y) = "[" ++ show x ++ " , " ++ show y ++ "["
intervalos (FF x y) = "[" ++ show x ++ " , " ++ show y ++ "]"
intervalos (Uniao x y) = "(" ++ show x ++ " U " ++ show y ++ ")"

--b
pertence :: Double -> SReais -> Bool
pertence x (AA a b) = x > a && x < b
pertence x (AF a b) = x > a && x <= b
pertence x (FA a b) = x >= a && x < b
pertence x (FF a b) = x >= a && x <= b
pertence x (Uniao a b) = pertence x a || pertence x b

--c
tira :: Double -> SReais -> SReais
tira x (AA a b) | pertence x (AA a b) = Uniao (AA a x) (AA x b)
tira x (AF a b) | pertence x (AF a b) = Uniao (AA a x) (AF x b)
tira x (FA a b) | pertence x (FA a b) = Uniao (FA a x) (AA x b)
tira x (FF a b) | pertence x (FF a b) = Uniao (FA a x) (AF x b)
tira x (Uniao a b) | pertence x a = Uniao (tira x a) b
                   | pertence x b = Uniao a (tira x b)

--EXERCÍCIO 3
data RTree a = R a [RTree a]

tree1 = R 1 [R 2 [],
             R 3 [R 4 [R 5 [],
                       R 6 []
                      ]
                 ],
             R 7 []
            ]

--a
percorre :: [Int] -> RTree a -> Maybe [a]
percorre [] (R a _) = Just [a]
percorre _ (R a  []) = Nothing 
percorre x@(h:t) (R a l) | length l < h || null aux3 =  Nothing
                         | otherwise = Just (a :aux3)
        where aux4 = percorre t (l !! (h-1))
              aux3 = fromMaybe [] aux4
--b
procura :: Eq a => a -> RTree a -> Maybe [Int]
procura n (R a l) | n == a =  Just []
                  | null l = Nothing
                  | otherwise = foldl (\acc num -> if procura n (l !! (num -1))== Nothing then acc else Just (num:fromMaybe [] (procura n (l!!(num-1))))) Nothing [1..length l]