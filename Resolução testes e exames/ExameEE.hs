module ExameEE1920 where

-----------------
---EXERCÍCIO 1---
-----------------

-----
 ----a
  -----

subst :: Eq a => (a,a) -> [a] -> [a]
subst _ [] = []
subst (x,y) (h:t) | x == h = y : subst (x,y) t
                  | otherwise = h : subst (x,y) t

-----
 ----b
  -----

posicoes:: [a] -> [Int] -> [a]
posicoes [] _ = []
posicoes _ [] = []
posicoes m [x] = [posicaoaux m x]
posicoes m (x:xs) = (posicaoaux m x) : (posicoes m xs)

posicaoaux :: [a] -> Int -> a
posicaoaux [] _ = error "Nada para devolver"
posicaoaux m x | x > length m = error "Posição enixestente para esta lista"
               | x == length m = (last m)
               | otherwise = posicaoaux (init m) x

-----------------
---EXERCÍCIO 2---
-----------------

data Tree a b = Leaf b | Node a (Tree a b) (Tree a b)

arv :: Tree Int Char
arv = Node 3 (Node 3 (Leaf 'B') 
                     (Node 2 (Leaf 'C') 
                             (Leaf 'r')
                     ))
              ((Node 5 (Leaf 'P') 
                      (Leaf 'L')
               ) 
              )

arv1 :: Tree Float Int
arv1 = Node 3.0 (Node 3.0 (Leaf 3) 
                     (Node 2.0 (Leaf 5) 
                             (Leaf 15)
                     ))
              ((Node 5.0 (Leaf 100) 
                      (Leaf 36)
               ) 
              )

-----
 ----a
  -----

folhas :: Tree a b -> [b]
folhas (Leaf x) = [x]
folhas (Node n e d) = folhas e ++ folhas d 


-----
 ----b
  -----

somas :: Tree Float Int -> (Float,Int)
somas (Leaf n) = (0,n)
somas (Node n e d) = let x = somanodes e + somanodes d
                         y = somaleaf e +somaleaf d
                     in (x + n, y)

somaleaf :: Tree Float Int -> Int
somaleaf (Leaf n) = n 
somaleaf (Node n e d) = somaleaf e + somaleaf d

somanodes :: Tree Float Int -> Float
somanodes (Leaf n) = 0
somanodes (Node n e d) = n + somanodes e +somanodes d


-----------------
---EXERCÍCIO 3---
-----------------

type Mat a = [[a]]

mat :: Mat Int
mat = [[1,2,3], [0,4,5], [0,0,6]]

rotateLeft :: Mat a -> Mat a
rotateLeft m = (map head m) : rotateLeft (map tail m)

-----------------
---EXERCÍCIO 4---
-----------------

type Filme = (Titulo,Realizador,[Actor],Genero,Ano)
type Titulo = String
type Realizador = String
type Actor = String
type Ano = Int

data Genero = Comedia | Drama | Ficcao | Accao | Animacao | Documentario
    deriving Eq

type Filmes = [Filme]

filme :: Filmes
filme = [("Vamos morrer", "A tua mãe", ["Ninguém1","Ninguém2","Ninguém3"], Animacao, 2021),("Vamos a recurso", "JBB", ["Eu","Bloco","Frade"], Drama, 2021),("A vida de um mosquito", "Beeeezz", ["Ninguém1","Ninguém2","Ninguém5"], Documentario, 1921)]

-----
 ----a
  -----

doRealizador :: Filmes -> Realizador -> [Titulo]
doRealizador [] _ = []
doRealizador (f@(t,r,_,_,_):xs) n | n == r = t : doRealizador xs n 
                                  | otherwise = doRealizador xs n

-----
 ----b
  -----

doActor :: Filmes -> Actor -> [Titulo]
doActor [] _ = []
doActor (f@(t,_,l,_,_):xs) a | elem a l = t : doActor xs a
                             | otherwise = doActor xs a

-----
 ----c
  -----

consulta :: Filmes -> Genero -> Realizador -> [(Ano, Titulo)]
consulta bd gen rea = map aux (filter (teste gen rea) bd)
  where teste :: Genero -> Realizador -> Filme -> Bool
        teste g r (_,x,_,y,_) = g==y && r==x

aux :: Filme -> (Ano,Titulo)
aux (t,_,_,_,a) = (a,t) 

-----------------
---EXERCÍCIO 5---
-----------------

data Avaliacao = NaoVi
                | Pontos Int

type FilmesAval = [(Filme,[Avaliacao])]

a :: FilmesAval
a = [(("Vamos morrer", "A tua mãe", ["Ninguém1","Ninguém2","Ninguém3"], Animacao, 2021),[NaoVi,2,4]),(("Vamos a recurso", "JBB", ["Eu","Bloco","Frade"], Drama, 2021),[NaoVi,10,3]),(("A vida de um mosquito", "Beeeezz", ["Ninguém1","Ninguém2","Ninguém5"], Documentario, 1921), [0,1,10])]

-----
 ----a
  -----

--avalia :: FilmesAval -> IO FilmesAval
--avalia 

-----
 ----b
  -----

listaPorGeneros :: FilmesAval -> [(Genero,[(Titulo,Avaliacao)])]
