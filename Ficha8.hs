module Ficha8 where

data Frac = F Integer Integer

--a 
normaliza :: Frac -> Frac
normaliza (F x y) = let x' = abs x
                        y' = abs y
                        c = mdc x' y'
                        s = (signum x)*(signum y)
                    in F (s*(div (abs x) c)) (div (abs y) c)

mdc :: Integer -> Integer -> Integer
mdc x y | x == y = x
        | x > y = mdc (x-y) y
        | x < y = mdc x (y-x)

 --b
instance Eq Frac where
     (F a b) == (F c d) = (a*d == c*b)
--c
instance Ord Frac where
     f1 <= f2 = let (F a b) = normaliza f1
                    (F c d) = normaliza f2
                in a*d <= c*d
--d
instance Show Frac where
--  show :: Frac -> String
    show (F a b) = "(" ++ show (a) ++ "/" ++ show (b) ++ ")"
--e
instance Num Frac where
{-    (+),(*),(-) :: a- > a -> a 
      negate, abs, signum :: a -> a
      fromInteger :: Integer -> a -}
      (F a b) + (F c d) = F (a*d+c*b) (b*d)
      (F a b) * (F c d) = F (a*c) (b*d)
      negate (F a b) = F (-a) b
      abs (F a b) = F (abs a) (abs b)
      signum (F a b) = F ((signum a)*(signum b)) 1
      fromInteger n = F n 1

fun :: Frac -> [Frac] -> [Frac]
fun f [] = []
fun f l = filter (> 2*f)  l

--EXERCÍCIO 2
data Exp a = Const a
           | Simetrico (Exp a)
           | Mais (Exp a) (Exp a)
           | Menos (Exp a) (Exp a)
           | Mult (Exp a) (Exp a)

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

--exemplos
ex1_ = Mais (Const 3) (Mult (Const 7) (Const 5))
ex2_ = Menos (Mais (Const 3) (Mult (Const 7) (Const 5))) (Mais (Const 10) (Const 20))
ex3_ = Mais (Const (F 25 5)) (Mult (Const (F 18 6)) (Const (F 25 5)))
--a            
instance Show a => Show (Exp a) where
     show  = infixa 
--b
instance (Eq a, Num a)=> Eq (Exp a) where
    a == b = (calcula a) == (calcula b)  
--c
instance Num (Exp a) where
    (+) = Mais
    (-) = Menos
    (*) = Mult
    signum =
    abs =
    fromInteger N = Const   n 