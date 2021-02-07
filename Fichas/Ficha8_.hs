module Ficha8 where

import Data.List
import Data.Char
--Exercício 1
data Frac = F Integer Integer

--a
normaliza :: Frac -> Frac
normaliza (F x y) = let x' = abs x
                        y' = abs y
                        c = mdc x' y'
                        s = (signum x)*(signum y)
                    in F (s*(div x' c)) (div y' c) 

mdc :: Integer -> Integer -> Integer
mdc x y | x == y = x
        | x > y = mdc (x-y) y
        | x < y = mdc x (y-x)

--b 
instance Eq Frac where
    (F a b) == (F c d) = (a*d == b*c)   

--c
instance Ord Frac where
    f1 <= f2 = let (F a b) = normaliza f1
                   (F c d) = normaliza f2
               in (a*d <= c*b)  
--d
instance Show Frac where
    show (F a b) = "(" ++ show (a) ++ "/" ++ show (b) ++ ")" 

--e 
instance Num Frac where
    (F a b) + (F c d) = F (a*d + c*b) (b*d)
    (F a b) * (F c d) =  F (a*c) (b*d)
    negate (F a b) =  F (-a) (b)
    abs (F a b) = F (abs a) (abs b)
    signum (F a b) = F ((signum a)*(signum b)) 1
    fromInteger n = F n 1

--f
fun :: Frac -> [Frac] -> [Frac]
fun _ [] = []
fun f l = filter (>2*f) l

--Exercício 2

data Exp a = Const a
           | Simetrico (Exp a)
           | Mais (Exp a) (Exp a)
           | Menos (Exp a) (Exp a)
           | Mult (Exp a) (Exp a)

ex1 = Mais (Const 3) (Mult (Const 7) (Const 5))

ex2 = Menos (Mais (Const 3) (Mult (Const 7) (Const 5))) (Mais (Const 10) (Const 20))


infixa :: Show a =>  Exp a -> String
infixa (Const x) = show x
infixa (Simetrico e) = "~" ++ (infixa e) 
infixa (Mais e1 e2) = "(" ++ (infixa e1) ++ "+" ++ (infixa e2) ++ ")"
infixa (Menos e1 e2) = "(" ++ (infixa e1) ++ "-" ++ (infixa e2) ++ ")"
infixa (Mult e1 e2) = "(" ++ (infixa e1) ++ "*" ++ (infixa e2) ++ ")"

calcula :: Num a => Exp a -> a
calcula (Const x) = x
calcula (Simetrico e) = calcula e
calcula (Mais e1 e2) = (calcula e1) + (calcula e2)
calcula (Menos e1 e2) = (calcula e1) - (calcula e2)
calcula (Mult e1 e2) = (calcula e1) * (calcula e2)

--a
instance Show a => Show (Exp a) where
    show = infixa

--b
instance (Eq a, Num a) => Eq (Exp a) where
    a == b = (calcula a) == (calcula b) 

--c
instance (Eq a, Num a) => Num (Exp a) where
    x+y = Const  ((calcula x) + (calcula y) )
    x-y = Const  ((calcula x) - (calcula y) )
    x*y = Const  ((calcula x) * (calcula y) )
    negate (Const a) = Const (-a)
    negate (Simetrico a) = abs a
    negate (Mais a b) = Mais (-a) (-b)
    negate (Menos a b) = Menos (-a) (-b)
    negate (Mult a b) = Mult (-a) (-b)
    fromInteger x = Const (fromInteger x)
    abs (Const a) = Const (abs a)
    abs (Simetrico a) =(abs a)
    abs (Mais a b) = abs (a+b)
    abs (Menos a b) = abs (a-b)
    abs (Mult a b) = abs (a*b)
    signum (Const a) = if (abs a) == a 
                       then if a == 0 
                            then 0
                            else 1
                        else -1 
    signum (Simetrico a) = - signum a                    
    signum (Mais a b) = if (abs (a+b)) == (a+b) 
                       then if (a+b) == 0 
                            then 0
                            else 1
                        else -1 
    signum (Menos a b) = if (abs (a-b)) == (a-b) 
                       then if (a-b) == 0 
                            then 0
                            else 1
                        else -1                     
    signum (Mult a b) = if (abs (a*b)) == (a*b) 
                       then if (a*b) == 0 
                            then 0
                            else 1
                        else -1 

--Exercício 3

data Movimento = Credito Float | Debito Float
data Data = D Int Int Int deriving Eq
data Extracto = Ext Float [(Data, String, Movimento)]

ex_1 :: Extracto
ex_1 = Ext 100 [(D 2020 1 30 , "compra", Debito 15), (D 2020 2 14, "salario", Credito 850)]

--a
instance Ord Data where
    compare (D d1 m1 a1) (D d2 m2 a2) | a1 > a2 || a1 == a2 && (m1 > m2 || m1==m2 && d1 > d2) = GT
                                      | a1 == a2 && m1==m2 && d1==d2 = EQ 
                                      | otherwise = LT

--b
instance Show Data where
    show (D d m a) = concat $ intersperse "/" $ map (show) [d,m,a] 

--c
instance Show Extracto where
    show (Ext n l) = "Saldo anterior : " ++ show n ++ 
                     "\n---------------------------------------" ++
                     "\nData       Descricao   Credito   Debito" ++
                     "\n---------------------------------------" ++
                     "\n" ++ concatMap (\(dat,str,mov) -> show dat ++ replicate (11 - (length (show dat))) ' ' ++ map (toUpper) str ++ "   \n") l++ 
                     "---------------------------------------" ++
                     "\nSaldo actual: " ++ show (saldo (Ext n l))

saldo :: Extracto -> Float
saldo (Ext n l) = foldl (\acc (_,_,mov) -> case mov of Credito n -> (acc + n)
                                                       Debito n -> (acc - n)) n l
