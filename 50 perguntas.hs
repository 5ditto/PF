module Revisao where

--1
myenumFromTo :: Int -> Int -> [Int]
myenumFromTo x y | x <= y = x : myenumFromTo (x+1) y
                 | otherwise = []


{-
myenumFromTo 1 5 = 1 : myenumFromTo 2 5
                 = 1 : 2 : myenumFromTo 3 5
                 = 1 : 2 : 3 : myenumFromTo 4 5
                 = 1 : 2 : 3 : 4 : myenumFromTo 5 5
                 = 1 : 2 : 3 : 4 : 5  = [1,2,3,4,5] -}

--2
myenumFromThenTo :: Int -> Int -> Int -> [Int]
myenumFromThenTo x y z | x <= z = x : myenumFromThenTo y (2*y-x) z
                       | otherwise = []

--3
p3 :: [a] -> [a] -> [a]
p3 [] l = l
p3 (x:xs) l = x : p3 xs l

--4
p4 :: [a] -> Int -> a
p4 [] _ = error "Não está definida"
p4 (x:xs) a | a == 0 = x
            | otherwise = p4 xs (a-1)

--5
myreverse :: [a] -> [a]
myreverse [] = []
myreverse l = last l : myreverse (init l)

p5 :: [a] -> [a]
p5 [] = []
p5 (x:xs) = (p5 xs) ++ [x]

--6
mytake :: Int -> [a] -> [a]
mytake _ [] = []
mytake 0 l = []
mytake n (x:xs) = x : mytake (n-1) xs 

p6 :: Int -> [a] -> [a]
p6 _ [] = []
p6 0 l = []
p6 n l | n >= length l = l
       | otherwise = p6 n (init l) 
--7
mydrop :: Int -> [a] -> [a]
mydrop _ [] = []
mydrop 0 l = l
mydrop n (x:xs) = mydrop (n-1) xs

p7 :: Int -> [a] -> [a]
p7 _ [] = []
p7 n (x:xs) | n ==0 = (x:xs) 
            | otherwise = p7 (n-1) xs
--8
myzip :: [a] -> [b] -> [(a,b)]
myzip [] [] = []
myzip [] l = []
myzip l [] = []
myzip (x:xs) (y:ys) = (x,y) : myzip xs ys

--9
myelem :: Eq a => a -> [a] -> Bool
myelem _ [] = False
myelem n (x:xs) | n == x = True
                | otherwise = myelem n xs

--10
myreplicate :: Int -> a -> [a]
myreplicate 0 x = []
myreplicate n x = x : myreplicate (n-1) x

--11
myintersperse :: a -> [a] -> [a]
myintersperse _ [] = []
myintersperse _ [x] = [x]
myintersperse n (x:xs) = x:n: myintersperse n xs    

--12  !!!
mygroup :: Eq a => [a] -> [[a]]
mygroup [] = [[]]
mygroup l  = aux12 l : mygroup (drop (length (aux12 l)) l)
 where aux12 :: Eq a => [a] -> [a]
       aux12 [] = []
       aux12 [a] = [a]
       aux12 (a:b:c) | a == b = a : (aux12 (b:c))
                     | otherwise = [a]
--13
myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat (x:xs) = x ++ myconcat (xs)

--14 !!!
myinits :: [a] -> [[a]]
myinits [] = [[]]
myinits l = myinits (init l) ++ [l]

--[1,2,3] = [0]++[1]++[1,2]++[1,2,3]

--15
mytails :: [a] -> [[a]]
mytails [] = [[]]
mytails (x:xs) = [x:xs] ++ mytails xs

--16
myisPrefixOf :: Eq a => [a] -> [a] -> Bool
myisPrefixOf [] l = True
myisPrefixOf l [] = False 
myisPrefixOf l t | elem  l (myinits t) = True
                 |otherwise = False

myisPrefixOf2 :: Eq a => [a] -> [a] -> Bool
myisPrefixOf2 [] l = True
myisPrefixOf2 l [] = False 
myisPrefixOf2 (x:xs) (y:ys) | x == y =  myisPrefixOf2 xs ys
                            | otherwise = False

--17
myisSuffixOf :: Eq a => [a] -> [a] -> Bool
myisSuffixOf [] l = True
myisSuffixOf l [] = False
myisSuffixOf l t | elem l (mytails t) = True
                 | otherwise = False 

myisSuffixOf2 :: Eq a => [a] -> [a] -> Bool
myisSuffixOf2 [] l = True
myisSuffixOf2 l [] = False
myisSuffixOf2 l t | last l == last t = myisSuffixOf2 (init l) (init t)
                  | otherwise = False 

--18
myisSubsequenceOf :: Eq a => [a] -> [a] -> Bool
myisSubsequenceOf [] l = True
myisSubsequenceOf l [] = False
myisSubsequenceOf (x:xs) (y:ys) | x == y = myisSubsequenceOf xs ys
                                | otherwise = myisSubsequenceOf (x:xs) ys  

--19
myelemIndices :: Eq a => a -> [a] -> [Int]
myelemIndices _ [] = []
myelemIndices n l | n == last l = myelemIndices n (init l) ++ [length l]
                  | otherwise = myelemIndices n (init l)

--20
mynub :: Eq a => [a] -> [a]
mynub [] = []
mynub (x:xs) = x : mynub (filter (/= x) xs)

--21
mydelete :: Eq a => a -> [a] -> [a]
mydelete _ [] = []
mydelete n (x:xs) | n == x = (xs)
                  | otherwise = x : mydelete n xs

--22
p22 :: Eq a => [a] -> [a] -> [a]
p22 [] _ = [] 
p22 l [] = l
p22 l (y:ys) = p22 (aux22 y l) ys
  where aux22 :: Eq a => a -> [a] -> [a]
        aux22 _ [] = []
        aux22 n (x:xs) | n == x = xs
                       | otherwise = x : aux22 n xs

--23
myunion :: Eq a => [a] -> [a] -> [a]
myunion [] l = l
myunion l [] = l
myunion l (x:xs) | elem x l = myunion l xs
                 | otherwise = myunion (l ++ [x]) xs

--24
myintersect :: Eq a => [a] -> [a] -> [a]
myintersect l [] = []
myintersect [] _ = []
myintersect (x:xs) l  | elem x l = x : myintersect xs l
                      | otherwise = myintersect xs l

--25
myinsert :: Ord a => a -> [a] -> [a]
myinsert x [] = [x]
myinsert n (x:xs) | x >= n = [n] ++ (x:xs)
                  | otherwise = x : myinsert n xs

--26
myunwords :: [String] -> String
myunwords [] = " "
myunwords [x] = x
myunwords (x:xs) = x ++ " " ++ myunwords xs

--27
myunlines :: [String] -> String
myunlines [] = []
myunlines (x:xs) = x ++ "\n" ++ myunlines xs 

--28
pMaior :: Ord a => [a] -> Int
pMaior [_] = 0
pMaior (x:xs) | x > (xs !! y) = 0
              | otherwise = 1 + y
        where y = pMaior xs

--29
temRepetidos :: Eq a => [a] -> Bool
temRepetidos [] = False 
temRepetidos (x:xs) | x `elem` xs = True
                    | otherwise = temRepetidos xs

--30
algarismos :: [Char] -> [Char]
algarismos [] = []
algarismos (x:xs) | x `elem`['0'..'9'] = x : algarismos xs
                  | otherwise = algarismos xs

--31
posImpares :: [a] -> [a]
posImpares [] = []
posImpares [_] = []
posImpares (x:y:xs) = [y] ++ posImpares xs

--32
posPares :: [a] -> [a]
posPares [] = []
posPares [x] = [x]
posPares (x:y:xs) = [x] ++ posPares xs

--33
isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:y:xs) | x > y = False 
                  | otherwise = isSorted (y:xs)

--34
iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (x:xs) = aux34 x (iSort xs)
   where aux34 :: Ord a => a -> [a] -> [a]
         aux34 x [] = [x]
         aux34 n (x:xs) | n >= x = [x] ++ (aux34 n xs)
                        | otherwise = (n:x:xs)

--35
menor :: String -> String -> Bool
menor _ "" = False
menor "" "" = False
menor "" _ = True
menor (x:xs) (y:ys) | x > y = False 
                    | x < y = True
                    | otherwise = menor xs ys

--36
elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet n [] = False 
elemMSet n ((x,y):xs) | x == n = True 
                      | otherwise = elemMSet n xs


--37
lengthMSet :: [(a,Int)] -> Int
lengthMSet [] = 0
lengthMSet ((_,y):xs) = y + lengthMSet xs

--38
converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((x,1):xs) = x : (converteMSet xs)
converteMSet ((x,y):xs) = x : (converteMSet ((x,y-1):xs))

--39
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet n [] = [(n,1)]
insereMSet n ((x,y):xs) | x == n = ((x, y+1):xs)
                        | otherwise = (x,y) : (insereMSet n xs)

--40
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet _ [] = []
removeMSet n ((x,1):xs) = xs
removeMSet n ((x,y):xs) | n == x = ((x, y-1) : xs)
                        | otherwise = (x,y) : removeMSet n xs

--41
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (x:xs) = (x, 1+ length (filter (==x) xs)) : constroiMSet (filter (/=x) xs)

constroiMSet2 :: Ord a => [a] -> [(a,Int)]
constroiMSet2 [] = []
constroiMSet2 (x:xs) = insereMSet x (constroiMSet2 xs)

--42
mypartitionEithers :: [Either a b] -> ([a],[b])
mypartitionEithers l = (aux42 l,aux42_ l )
  where aux42 :: [Either a b] -> [a]
        aux42 [] = []
        aux42 ((Left x):xs) = x:aux42 xs
        aux42 ((Right x):xs)= aux42 xs

        aux42_ :: [Either a b] -> [b]
        aux42_ [] = []
        aux42_ ((Left x):xs) = aux42_ xs
        aux42_ ((Right x):xs) = x : aux42_ xs 

--43
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (x:xs) = case x of Nothing -> catMaybes xs
                             Just l -> l:catMaybes xs 

--44
data Movimento = Norte | Sul | Este | Oeste
    deriving Show

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao (x,y) [] = (x,y)
posicao (x,y) (a:as) = posicao ( case a of Norte -> (x,y+1)
                                           Sul -> (x,y-1)
                                           Este -> (x+1,y)
                                           Oeste -> (x-1,y)) as

--45
caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (x,y) (a,b) | a > x = Este : caminho (x+1,y) (a,b)
                    | a < x = Oeste : caminho (x-1,y) (a,b)
                    | b > y = Norte : caminho (x,y+1) (a,b)
                    | b < y = Sul : caminho (x,y-1) (a,b)
                    | otherwise = [] 

--46
vertical :: [Movimento] -> Bool
vertical [] = True
vertical (x:xs) = case x of Este -> False
                            Oeste -> False
                            otherwise -> vertical xs

--47
data Posicao = Pos Int Int
   deriving Show

maisCentral :: [Posicao] -> Posicao
maisCentral [(Pos x y)] = Pos x y
maisCentral ((Pos x y): (Pos a b):xs) | (x^2+y^2) < (a^2+b^2) = maisCentral((Pos x y) :xs)
                                      | otherwise = maisCentral ((Pos a b) : xs)

--48
vizinhos :: Posicao -> [Posicao] -> [Posicao]
vizinhos x [] = []
vizinhos (Pos x y) ((Pos a b):xs) | abs (x-a) == 1 && y == b || abs (y-b) == 1 && x==a = (Pos a b) : vizinhos (Pos x y) xs
                                  | otherwise = vizinhos (Pos x y) xs

--49
mesmaOrdenada :: [Posicao] -> Bool
mesmaOrdenada [] = error "Não definido"
mesmaOrdenada [(Pos x y)] = True
mesmaOrdenada ((Pos x y):(Pos a b):xs)| y == b = mesmaOrdenada ((Pos a b):xs) 
                                      | otherwise = False


--50
data Semaforo = Verde | Amarelo | Vermelho
  deriving Show

interseccaoOK :: [Semaforo] -> Bool
interseccaoOK l | (length $ aux50 l) > 1 = False
                | otherwise = True
     where aux50 :: [Semaforo] -> [Semaforo]
           aux50 [] = []
           aux50 (Vermelho:xs) = aux50 xs
           aux50 (x:xs) = x:(aux50 xs)