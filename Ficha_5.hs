module FICHA5 where

--EXERCÍCIO 1
--f
deleteBy' :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy' _ _ [] = []
deleteBy' f x (h:t) | f x h = t
                    | otherwise = h : deleteBy' f x t

--g
sortOn :: Ord b => (a->b) -> [a] -> [a]
sortOn _ [] = []
sortOn f (x:xs) = aux f x (sortOn f xs)
-- auxiliar insere, de forma ordenada, o 1º elemento na cauda da lista (ordenada)

aux :: Ord b => (a->b) -> a -> [a] -> [a]
aux f x [] = [x] 
aux f x (m:ms) | f x <= f m = x : (m:ms)
               | f x > f m = m : aux f x ms

--EXERCÍCO 2 
type Polinomio = [Monomio]
type Monomio = (Float,Int)

ex1 :: Polinomio
ex1 = [(2,3),(3,4),(5,3),(4,0)]
--a 
selgrau :: Int -> Polinomio -> Polinomio
selgrau _ [] = []
selgrau n m = filter (\(c,e) -> n == e) m

--b
conta :: Int -> Polinomio -> Int
conta n [] = 0
conta n p = length (selgrau n p)

--c
grau :: Polinomio -> Int 
grau p = foldr (\(c,e) g -> max e g) 0 p

--d
deriv :: Polinomio -> Polinomio
deriv p = map (\(c,e) -> (c*( fromIntegral e), e-1)) p 

--e
calcula :: Float -> Polinomio -> Float
calcula n p = (sum (map (\(c,e) -> (c*(n^e))) p))

--f
simp :: Polinomio -> Polinomio
simp p = filter (\(c,e) -> e /= 0 ) p

--g
mult :: Monomio -> Polinomio -> Polinomio
mult (a,b) p = foldr (\(c,e) r -> (a*c,b+e) : r) [] p

--h 
ordena :: Polinomio -> Polinomio
ordena = sortOn (snd) 
               -- serve para ordenar de acordo com o grau

--i
normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza ((x,y):xs) = (sum [ a|(a,b) <- selgrau y xs ]+x , y) : normaliza [(c,d) | (c,d) <- xs, d/=y ]

--j 
soma :: Polinomio -> Polinomio -> Polinomio
soma [] p = normaliza p
soma p [] = normaliza p
soma m p = normaliza (m++p)

--k
produto :: Polinomio -> Polinomio -> Polinomio
produto p l = foldr (\m r -> soma (mult m p) r) [] l

--l 
equiv :: Polinomio -> Polinomio -> Bool
equiv [] [] = True
equiv _ [] = False
equiv [] _ = False
equiv m p | normaliza m == normaliza p = True
          | otherwise = False

--EXERCÍCIO 3
type Mat a = [[a]]

ex2 = [[1,2,3], [0,4,5], [0,0,6]]
ex3 = [[5,8,13],[32,48,05],[0,0,1]]
--a 
dimOk :: Mat a -> Bool
dimOk [] =  False
dimOk m = aux (map length m)
    where aux (0:_) = False
          aux (n:xs) = filter (/=n) xs == []

--b     (nº linhas,nº colunas)
dimMat :: Mat a -> (Int,Int) 
dimMat [] = (0,0)
dimMat m@(h:y) = (length m,length h)

--c
addMat :: Num a => Mat a -> Mat a -> Mat a
addMat [] [] = []
addMat (h:t) (x:y) = zipWith (+) h x : addMat t y

--d 
transpose :: Mat a -> Mat a
transpose ([]:_) = []
transpose m = (map head m) : transpose (map tail m)

--e
multMat :: Num a => Mat a -> Mat a -> Mat a
multMat _ [] = []
multMat [] _ = []
multMat (x:y) p = aux_ x p : multMat y p
   where aux_ :: Num a => [a] -> Mat a -> [a]
         aux_ m ([]:_) = []
         aux_ m p = sum (zipWith (*) m (map head p)) : (aux_ m (map tail p))

--f
zipWMat :: (a -> b -> c) -> Mat a -> Mat b -> Mat c
zipWMat f (x:xs) (y:ys) = zipWith f x y : zipWMat f xs ys
zipWMat _ _ _ = []

--g
triSup :: Num a => Mat a -> Bool
triSup [] = False
triSup m 

--h
rotateLeft :: Mat a -> Mat a
rotateLeft ([]:_) = []
rotateLeft m = (map last m) : rotateLeft (map init m)
