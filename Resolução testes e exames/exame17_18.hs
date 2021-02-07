module EXAME16_17 where


--1
(!!!) :: [a] -> Int -> a 
(!!!) [] _ = error "Impossível"
(!!!) (x:xs) n | n == 0 = x 
               | otherwise = (!!!) xs (n-1)

--2
data Movimento = Norte | Sul | Este | Oeste 
     deriving Show

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao (x,y) [] = (x,y)
posicao (x,y) (m:ms) = posicao (aux (x,y) m) ms
  where aux (x,y) Norte = (x,y+1)
        aux (x,y) Sul = (x,y-1)
        aux (x,y) Este = (x+1,y)
        aux (x,y) Oeste = (x-1,y)

--3
any' :: (a -> Bool) -> [a] -> Bool
any' n [] = False
any' n (x:xs) =  n x || any' n xs 

--4
type Mat a = [[a]]

ex = [[1,2,3], [0,4,5], [0,0,6]]

--triSup :: Num a => Mat a -> Bool
--triSup [[]] = False
--triSup m | (lenght m) == (lenght (head m)) = aux m 0
--         | otherwise = False

--aux :: Mat a -> Int -> Bool
--aux m n | take n m == replicate n 0 =  aux (tail m) (n+1)
--        | otherwise = False

--5
