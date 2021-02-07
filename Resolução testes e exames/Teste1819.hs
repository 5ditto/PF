module Teste19 where

import System.Random

--EXERCÍCIO 1
--a
elemIndices

--EXERCÍCIO 5
type Mat a = [[a]]

m = [[6,7,2], [1,5,9], [8,3,4]]

--a
getElem :: Mat a -> IO a
getElem m = do l <- randomRIO (0,(length m)-1)
               c <- randomRIO (0,(length (head m))-1)
               return (select l c m)

select :: Int -> Int -> Mat a -> a
select l c m = (m!!l)!!c


