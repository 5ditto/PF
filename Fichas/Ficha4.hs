module Ficha4 where

import Data.Char

--EXERCÍCIO 1
--a
--[6,12,18]
--b
--[6,12,18]
--c
--[(10,20),(11,19),(12,18),(13,17),(14,16),(15,15),(16,14),(17,13),(18,12),(19,11),(20,10)]
--d
--[1,1,4,](NÃO PERCEBI)

--EXERCÍCIO 2
--a
--[2^x | x <- [1..10]]
--b
--[(x,y) | x <- [1..5], y <- [1..5], x+y==6]
--c
--
--d
--[replicate [y | y <- [1..5]] | x <- [1] ]
--e

--EXERCÍCIO 3
digitAlpha :: String -> (String,String)
digitAlpha s = foldl (\(letra,num) x -> if isDigit x then (letra, num ++ [x]) 
                                        else if isAlpha x then (letra ++ [x], num)
                                             else (letra, num)) ("","") s

--EXERCÍCIO 4
nzp :: [Int] -> (Int,Int,Int)
nzp = foldl (\(n,z,p) x -> if x < 0 then (n+1,z,p) 
                           else if (x > 0) then (n,z,p+1)
                           else (n,z+1,p)) (0,0,0) 

--EXERCÍCIO 5
divMod0 :: Integral a => a -> a -> (a,a)
divMod0 x y = foldl(\(a,b) x -> (a+1,b-y )) (0,x) [y,2*y..x]

--EXERCÍICO 6
fromDigits0 :: [Int] -> Int
fromDigits0 = foldl (\ac a -> a + 10*ac) 0 

--EXERCÍCIO 8
