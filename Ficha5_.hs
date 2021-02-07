module Ficha5 where

--EXERCÍCIO 1
--a
any' :: (a -> Bool) -> [a] -> Bool
any' n [] = False
any' n (x:xs) = n x || any' n xs

--b
zipWith' :: (a->b->c) -> [a] -> [b] -> [c]
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
zipWith' _ _ _ = []

--c
takeWhile' :: (a->Bool) -> [a] -> [a]
takeWhile' f [] = []
takeWhile' f (x:xs) | f x  = x : takeWhile' f xs
                    | otherwise = []

--d
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' f [] = []
dropWhile' f (x:xs) | f x  =dropWhile' f xs
                    | otherwise = (x:xs) 

--e
span' :: (a->Bool) -> [a] -> ([a],[a])
span' f [] = ([],[])
span' f (x:xs) | f x = (x:a,b)
               | otherwise = ([],x:xs)
   where (a,b) = span' f xs

--f
deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy f n [] = []
deleteBy f n (x:xs) | f n x = xs
                    | otherwise = x : deleteBy f n xs

--g 
sortOn :: Ord b => (a->b) -> [a] -> [a]
sortOn f [] = []
sortOn f (x:xs) = insert f x (sortOn f xs)

insert :: Ord b => (a->b) -> a -> [a] -> [a]
insert f x [] = [x]
insert f n (x:xs) | f n <= f x = n:x:xs
                  | otherwise = x : insert f n xs

--EXERCÍCIO 2
type Polinomio = [Monomio]
type Monomio = (Float,Int)

ex1 :: Polinomio
ex1 = [(2,3),(3,4),(5,3),(4,5),(0,7)]

--a
selgrau :: Int -> Polinomio -> Polinomio
selgrau g p = filter (\(c,e) -> g == e) p

--b
conta :: Int -> Polinomio -> Int
conta n p = length $ filter(\(c,e) -> n == e) p

--c
grau :: Polinomio -> Int
grau p = foldr (\(c,e) g -> max e g) 0 p

--d
deriv :: Polinomio -> Polinomio
deriv p = map (\(c,e) -> (c*(fromIntegral e),e-1)) p

--e
calcula :: Float -> Polinomio -> Float
calcula n p = sum (map (\(c,e) -> (c*(n^e))) p)

--f
simp :: Polinomio -> Polinomio
simp p = filter (\(c,e) -> c /= 0) p

--g
mult :: Monomio -> Polinomio -> Polinomio
mult(a,b) p = foldr (\(c,e) r -> (a*c, b+e) : r ) [] p 

--h
ordena :: Polinomio -> Polinomio
ordena = sortOn (snd)

--i
normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza ((x,y):xs) = (sum [a | (a,b) <- selgrau y xs]+ x,y) : normaliza [(c,d)| (c,d)<- xs, d/=y ]

--j
soma :: Polinomio -> Polinomio -> Polinomio
soma [] p = p
soma p [] = p
soma ((x,y):xs) ((a,b):c) = normaliza (((x,y):xs)++((a,b):c))

--k
produto :: Polinomio -> Polinomio -> Polinomio
produto p l = foldr  (\m r -> soma (mult m p) r ) [] l

--l 
equiv :: Polinomio -> Polinomio -> Bool
equiv [] [] = True
equiv [] p = False
equiv p [] = False
equiv p h | normaliza (ordena p) == normaliza (ordena h) = True
          | otherwise = False

--EXERCÍCIO 3
type Mat a = [[a]]

exa = [[1,2,3],[0,4,5],[0,0,6]]
exb = [[5,8,13],[32,48,05],[0,0,1]]

--a
dimOK :: Mat a -> Bool
dimOK [] = False
dimOK m = aux (map length m)
   where aux (0:_) = False
         aux (n:x) = filter (/=n) x ==  []

--b          
dimMat :: Mat a -> (Int,Int)
dimMat [] = (0,0)
dimMat m@(l:ls) = (length m, length l)

--c
addMat :: Num a => Mat a -> Mat a -> Mat a
addMat (h:t) (x:xs) = zipWith (+) h x : addMat t xs 
addMat [] [] = []

--d
transpose :: Mat a -> Mat a
transpose ([]:_) = []
transpose m = (map head m) : transpose (map tail m)

--e
multMat :: Num a => Mat a -> Mat a -> Mat a
multMat 