module Exame18_19 where

----1
--a
isSorted :: (Ord a) => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:y:xs) | x <= y = isSorted (y:xs)
                  | x > y = False 

--b
inits :: [a] -> [[a]]
inits [] = [[]]
inits (x:xs) = 