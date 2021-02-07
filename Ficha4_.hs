module Ficha4 where

import Data.Char
import Data.List

--EXERCÍCIO 1
--a [6,12,18]
--b [6,12,18]
--c [(10,20),(11,19),(12,18),(13,17),(14,16),(15,15),(16,14),(17,13),(18,12),(19,11),(20,10)]
--d [1,1,4,4,9,9,16,16,25,25]

--EXERCÍCIO 2
--a [2^x | x <- [0..10]]

--b [(x,y)| x <-[1..5] , y <- [1..5] , x+y == 6]

--c [[1..x] | x <- [1..5] ]

--d [replicate x 1 | x <- [1..5]]

--e [product [1..x] | x <- [1..6]]

--EXERCÍCIO 3

digitAlpha :: String -> (String,String)
digitAlpha [] = ([],[])
digitAlpha m = (aux m, aux_ m)
 where aux :: String -> String
       aux [] = []
       aux (x:xs) | isDigit x = x : aux xs
                  | isAlpha x = aux xs
       aux_ :: String -> String
       aux_ [] = []
       aux_ (x:xs) | isDigit x = aux_ xs
                   | isAlpha x = x : aux_ xs

digitAlpha' :: String -> (String,String)
digitAlpha' [] = ([],[])
digitAlpha' m = (filter (isDigit) m, filter (isAlpha) m)

--Exercício 4
nzp :: [Int] -> (Int,Int,Int)
nzp [] = (0,0,0)
nzp l = (aux4 (filter (<0) l),aux4 (filter (== 0) l),aux4 (filter (>0) l))
  where aux4 [] = 0
        aux4 (h:t) = 1 + aux4 t

--Exercício 5
divMod_ :: Integral a => a -> a -> (a, a)
divMod_ _ 0 = error "És burro?, Isto é impossível :)"
divMod_ x y = (aux5div x y, aux5mod x y)
   where 
    aux5div x y | x-y < y = 0
                | x-y >= y = 1+ aux5div (x-y) y

    aux5mod x y | x-y > y = aux5mod (x-y) y
                | x-y == y = 0
                | x-y < y = x

--Exercício 6

fromDigits' :: [Int] -> Int
fromDigits' [] = 0
fromDigits' (h:t) = h*10^(length t) + fromDigits' t

fromDigits :: [Int] -> Int
fromDigits m = foldl (\x a -> a + 10*x) 0 m

--Exercício 7

maxSumInit' :: (Num a, Ord a) => [a] -> a
maxSumInit' l = maximum [sum m | m <- inits l]

-- [1,2,-2,4] = 1 + [2,-2,4] = 3 + []
maxSumInit :: (Num a, Ord a ) => [a] -> a
maxSumInit [] = error "ÉS BURRO? É IMPOSSÍVEL :)"
maxSumInit l = foldl (\x a -> max (sum a) x) 0 (inits l)
--                    \acc 1º elemento

--Exercício 8
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fib' :: Int -> Int
fib' n = auxFib n (0,1)
  where auxFib 0 (a,b) = a
        auxFib 1 (a,b) = b
        auxFib n (a,b) = auxFib (n-1) (b,a+b)