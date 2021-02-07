module Questoes where

--EXERCÍCIO 1
enumFromTo' :: Int -> Int -> [Int]
enumFromTo' x y | y >= x = x : enumFromTo' (x+1) y
                | y < x = []

--EXERCÍCIO 2
enumFromThenTo' :: Int -> Int -> Int -> [Int]
enumFromThenTo' x y z | x > z = if y > z then x : enumFromThenTo' y (2*y-x) z else [x]
                      | x < z = if y > z then x : enumFromThenTo' y (2*y-x) z else [x]
                      | x == z = [x]
