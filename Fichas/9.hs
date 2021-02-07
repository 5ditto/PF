module FIcha9 where

import System.Random 

--randomIO : gera um valor aleatório do tipo a
--dandomRIO gera um valor aleatório do tipo a dentro de uma determinada gama de valores

--a
bingo :: I0 ()
bingo = do l <- geralista 90 [1..90]
           apresenta l

geralista :: Int -> [Int] -> IO [Int]
geralista 0 _ = return []
geralista n l = do p <- randomRIO (0,n-1)
                   let (l1,x:xs) = splitAt p l
                   m <- geralista (n-1) (l1++xs)
                   return (x:m)

apresenta :: [Int] -> IO ()
apresenta [] = putStrLn "FIM"
apresenta (x:xs) = do putStrLn "Prima ENTER"
                      getChar
                      print x
                      apresenta xs

getCh :: IO Char
getCh = do hSetEcho stdin False
           s <- getChar
           hSetEcho stdin False
           return x

