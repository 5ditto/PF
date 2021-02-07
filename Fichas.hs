module Fichas where

---FICHA 1
--1
--a
perimetro :: Double -> Double
perimetro x = 2 * (pi) * x

--b
dist :: (Double,Double) -> (Double, Double) -> Double
dist (x,y) (a,b) = sqrt ((a-x)^2 + (b-y)^2)

--c
primUlt :: [a] -> (a,a)
primUlt l = (head l, last l)

--d
multiplo :: Int -> Int -> Bool
multiplo x y = mod x y == 0

--e
truncaImpar :: [a] -> [a]
truncaImpar l | even (length l) = l
              | otherwise = (tail l)

--f
max2 :: Int -> Int -> Int
max2 x y | x > y = x
         | x < y = y 
         | x == y = error "Os números são iguais"

--g
max3 :: Int -> Int -> Int -> Int
max3 x y z = max2 (max2 x y) z

--2
--a
nRaizes :: Float -> Float -> Float -> Float
nRaizes x y z | (y^2-4*x*z) > 0 = 2
              | (y^2-4*x*z) < 0 = 0
              | (y^2-4*x*z) ==0 = 1

--b
raizes :: Float -> Float -> Float -> [Float]
raizes x y z | (nRaizes x y z) == 0 = error "não tem soluções"
             | (nRaizes x y z) == 1 = [((-y)/(2*x)) ]
             | (nRaizes x y z) == 2 = [(((-y)+ sqrt(y^2-4*x*z))/(2*x)),(((-y)-sqrt(y^2-4*x*z))/(2*x))]

--3
type Hora = (Int,Int)

--a
testahora :: Hora -> Bool
testahora (x,y) = x >= 0 && x <= 23 && y >= 0 && y <= 60

--b
comparahora :: Hora -> Hora -> Bool 
comparahora (x,y) (a,b) = (a > x) || (x==a) && (b > y) 

--c
convertemin :: Hora -> Int
convertemin (x,y) = (x*60)+y

--d
convertehora :: Int -> Hora
convertehora x = (div x 60, mod x 60)