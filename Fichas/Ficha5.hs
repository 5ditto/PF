module FICHA5 where

-----EXERCÍCIO 1
--a
myany :: (a -> Bool) -> [a] -> Bool
myany n [] = False
myany n (x:xs) = if n x
               then True
               else myany n xs

any' :: (a -> Bool) -> [a] -> Bool
any' n [] = False
any' n (x:xs) = n x || any' n xs



---f
deleteBy :: (a -> a -> Bool) -> a  -> [a] -> [a]
deleteBy p x [] = []
deleteBy p x (y:ys) | p x y = ys
                    | otherwise = y : deleteBy p x ys

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f [] = []
sortOn f (x:xs) = insert f x (sortOn f xs)

insert :: Ord b => (a -> b) -> a -> [a] -> [a]
insert f x [] = [x]
insert f x (y:ys) | f x <= f y = x:y:ys
                  | otherwise = y : insert f x ys

-----EXERCÍCIO 2

type Polinomio = [Monomio]
type Monomio = (Float,Int)

ex1 :: Polinomio
ex1 = [(2,3), (3,4), (5,3), (4,5)]

--a
selgrau :: Int -> Polinomio -> Polinomio
selgrau g p = filter (\(c,e) -> g==e ) p

--b
conta :: Int -> Polinomio -> Int
conta n p = length (selgrau n p)

--ou sem ordem superior
{-
conta :: Int -> Polinomio -> Int
conta _ [] = 0
conta n ((x,y):xs) | n == y = 1 + conta n xs
                   | otherwise = conta n xs 
-}
--myconta :: Int -> Polinomio -> Int
--myconta n p | 

--c
--APENAS COM EXPOENTES POSITIVOS
grau :: Polinomio -> Int
{- RECURSIVA
grau [] = 0
grau ((x,y):ys) = max y (grau ys)
-}
grau p = foldr (\(x,y) r -> max y r) 0 p

--d
deriv :: Polinomio -> Polinomio
deriv p = map (\(x,y) -> (x*(fromIntegral y),y-1)) p

--e
calcula :: Float -> Polinomio -> Float
calcula n p = foldr (\(x,y) r -> (x*(n^y))+r) 0 p

--ou
calcula' :: Float -> Polinomio -> Float
calcula' n p = sum (map (\(x,y) -> (x*(n^y))) p)  

--g
mult :: Monomio -> Polinomio -> Polinomio
mult (a,b) p = foldr (\(x,y) r -> (a*x,b+y):r) [] p

--ou 
mul :: Monomio -> Polinomio -> Polinomio
mul (a,b) p = map (\(x,y) -> (a*x,b+y)) p

--k 
{-produto :: Polinomio -> Polinomio -> Polinomio
produto  p l = foldr (\m r -> sum ( mult m p) r) [] l
-}
--EXERCÍCIO 3
type Mat a = [[a]]

mat1 = [[3,4],[5,6],[7,8]]
mat2 = [[3,7],[4,1],[7,1]]

--a
dimOk :: Mat a -> Bool
dimOk [] = False
dimOk m = aux (map length m)
 where aux (0:_) = False
       aux (x:xs) = (filter (/=x) xs) == []

--b
dimMat :: Mat a -> (Int,Int)
dimMat m@(l:ls) = (length m , length l)

--c
addMat :: Num a => Mat a -> Mat a -> Mat a
addMat (m:ms) (x:xs) = zipWith (+) m x : addMat ms xs
addMat [] [] = []

--d
transpose :: Mat a -> Mat a
transpose ([]:_) = []
transpose m = (map head m) : transpose (map tail m)

--e
multMat :: Num a => Mat a -> Mat a -> Mat a
multMat (x:xs) m = ( linhas x m) : multMat xs m
multMat [] _ = []

linhas :: Num a => [a] -> Mat a -> [a]
linhas l ([]:_) = []
linhas l m = sum (zipWith (*) l (map head m)) : (linhas l (map tail m))

--f
zipWMat ::(a-> b -> c) -> Mat a -> Mat b -> Mat c
zipWMat f (x:xs) (y:ys) = zipWith f x y : zipWMat f xs ys
zipWMat _ _ _ = []

somaMat  :: Num a => Mat a -> Mat a -> Mat a
somaMat a b = zipWMat (+) a b