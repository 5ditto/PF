module TESTE18_19 where

import Data.Char
import System.Random

--EXERCÍCIO 1
--a
elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices _ [] = []
elemIndices n l | n == (last l) =  elemIndices n (init l) ++ [length l-1]
                | otherwise = (elemIndices n (init l))

--b
isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf (x:xs) (y:ys) | x == y = isSubsequenceOf xs ys
                              | otherwise =isSubsequenceOf xs (y:ys)

--EXERCÍCIO 2
data BTree a = Empty | Node a (BTree a) (BTree a)
  deriving (Show)

arv = (Node (5,9) (Node (7,3) (Node (3,5) Empty Empty)
                      (Node (2,5) (Node (10 ,0)Empty Empty) Empty)
              )
              (Node (2,86) (Node (12,9) Empty Empty)
                      (Node (4,5) Empty (Node (5,5) Empty Empty))
              )
      )

arv_ = (Node 5 (Node 7 (Node 3 Empty Empty)
                      (Node 2 (Node 10 Empty Empty) Empty)
              )
              (Node 1 (Node 12 Empty Empty)
                      (Node 4 Empty (Node 8 Empty Empty))
              )
      )


--a
lookupAP :: Ord a => a -> BTree (a,b) -> Maybe b
lookupAP x Empty = Nothing
lookupAP x (Node (y,z) e d) | x == y = Just z
                            | x > y = lookupAP x e
                            | x < y = lookupAP x d

--b
zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f (Node a e d) (Node b e1 d1) = Node (f a b) (zipWithBT f e e1) (zipWithBT f d d1)
zipWithBT _ _ _ = Empty

--EXERCÍCIO 3

digitAlpha :: String -> (String,String)
digitAlpha [] = ([],[])
digitAlpha (x:xs) | isDigit x = (x:m,n)
                  | isAlpha x = (m,x:n)
           where (m,n) = digitAlpha xs

--EXERCÍCIO 4
data Seq a = Nil | Cons a (Seq a) | App (Seq a) (Seq a)


ex = (App )
--a
firstSeq :: Seq a -> a
firstSeq (Cons n t) = n
firstSeq (App e d) | temNIL e = firstSeq d
                   | otherwise =firstSeq e

temNIL :: Seq a -> Bool
temNIL Nil = True
temNIL (Cons x s) = False
temNIL (App e d) = temNIL e && temNIL d 

--b

--c
instance Show a => (Show (Seq a))where
    show (n) = "<<" ++ init (aux n) ++ ">>"

aux :: Show a => Seq a -> String
aux (App e d) = (aux e) ++ (aux d)
aux (Cons n x) = show n ++ "," ++ (aux x)
aux (Nil) = ""

--EXERCÍCIO 4
type Mat a = [[a]]

m :: Mat Int
m = [[6,7,2], [1,5,9], [8,3,4]]

--a
getElem :: Mat a -> IO a
getElem m = do l <- randomRIO (0,(length m) -1)
               c <- randomRIO (0,(length (head m))-1)
               return (select l c m)

select :: Int -> Int -> Mat a -> a
select l c m = ((m!!l)!!c)

--b
magic :: Mat Int -> Bool 
magic m = somalinhas n m && somacolunas n m && somadiagonais n m
    where n = sum (head m)


somalinhas :: Int ->  Mat Int -> Bool 
somalinhas n  = foldl (\acc l -> sum l == n && acc) True 

somacolunas :: Int -> Mat Int -> Bool
somacolunas n m = foldl (\acc x -> sum (map (\l -> l !! x) m) == n && acc) True [0..(length m - 1)]

somadiagonais :: Int -> Mat Int -> Bool
somadiagonais n m = sum (map (\n -> (m !! n) !! n) [0..ln]) == n && sum (map (\n -> (m !! n) !! (ln - n)) [0..ln]) == n
    where ln = length m - 1