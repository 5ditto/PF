module Exame19 where

--EXERCÍCIO 5
type RelP a = [(a,a)]
type RelL a = [(a,[a])]
type RelF a = ([a], a -> [a])	

a :: RelP Int
a = [(1,3),(1,4),(2,1),(2,4),(2,5),(3,7),(4,7),(5,7),(6,5),(7,6)]
b :: RelL Int
b = [(1,[3,4]),(2,[1,4,5]),(3,[7]),(4,[7]),(5,[7]),(6,[5]),(7,[6])]

--a (sei fazer recursivamente)
convPL :: (Eq a) => RelP a -> RelL a 
convPL [] = []
convPL ((x,y):xs) = let (lx,tx) = filtra x t
                    in  (x,y:lx) : convPL tx

filtra :: (Eq a) => a -> RelP a -> ([a],RelP a)
filtra n [] = ([],[])
filtra n ((x,y):xs) = let (lx,tx) = filtra n xs
                      in if n == x 
                         then (y:lx,tx)
                         else (lx,(x,y):tx)


--b
criaRelPint :: Int -> IO (RelP Int)
criaRelPint n = do putStr "Insira um par de inteiros:"
                s <- getLine
                l <- criaRelPint (n-1)
                return ((read s):l)

convFP :: (Eq a) => RelF a -> RelP a
convFP (l,g) = convPL (map (\x -> (x,g x)) l)

convPF :: (Eq a) => RelP a -> RelF a
convPF l = (nub ((map fst l)++ (map snd l)), fun l)
    where fun :: RelP a -> a -> [a]
          fun l x = map snd (filter (\(y,_)->y==x) l)


data BTree a = Empty | Node a (BTree a) (BTree a)
  deriving Show

lookupAp :: Ord a => a -> BTree (a,b) -> Maybe a
lookupAp x Empty = Nothing
lookupAp x (Node (y,z) e d) | x == y = Just z
                            | x < y  = lookupAp x e
                            | x > y  = lookupAp x d

zipWithBT :: (a->b->c) -> BTree a -> BTree b -> BTree c
zipWithBT f (Node x e d) (Node t e' d') = Node (f x y) (zipWithBT f e e') (zipWithBT f d d')
zipWithBT _ _ _ = Empty