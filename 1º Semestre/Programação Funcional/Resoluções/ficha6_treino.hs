data BTree a = Empty
            | Node a (BTree a) (BTree a)
        deriving Show

-- Árvore de exemplo para testar as funções 
a = (Node 5 (Node 2 (Node 1 Empty
                                 Empty) 
                         (Node 3 Empty 
                                 Empty)) 
                 (Node 9 (Node 7 (Node 6 Empty 
                                         Empty) 
                                 (Node 8 Empty 
                                         Empty)) 
                         Empty))

-- Exercício 1

altura :: BTree a -> Int
altura Empty = 0
altura (Node x e d) = 1 + max (altura e) (altura d)

contaNodos :: BTree a -> Int
contaNodos Empty = 0
contaNodos (Node x e d) = 1 + contaNodos e + contaNodos d

folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node _ Empty Empty) = 1
folhas (Node _ e d) = folhas e + folhas d

prune :: Int -> BTree a -> BTree a
prune _ Empty = Empty
prune 0 _ = Empty
prune x (Node e l r) = Node e (prune (x - 1) l) (prune (x - 1) r)

path :: [Bool] -> BTree a -> [a]
path _ Empty = []
path [] (Node a _ _) = [a]
path (x:xs) (Node a e d) 
        | x == False = a :path xs e
        | otherwise = a :path xs d

mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node a e d) = (Node a (mirror d) (mirror e))

zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT _ Empty _ = Empty
zipWithBT _ _ Empty = Empty
zipWithBT o (Node a1 e1 d1) (Node a2 e2 d2) = (Node (o a1 a2) (zipWithBT o e1 e2) (zipWithBT o d1 d2)) 

unzipBT :: BTree (a,b,c) -> (BTree a, BTree b, BTree c)
unzipBT Empty = (Empty, Empty, Empty)
unzipBT (Node (a,b,c) e d) =
        (Node a e1 d1
        Node b e2 d2
        Node c e3 d3)
        where
           (e1,e2,e3) = unzipBT e
           (d1,d2,d3) = unzipBT d

-- Exercico 2
minimo :: Ord a => BTree a -> a
minimo (Node a e Empty) = minimo e
minimo (Node a Empty Empty) = a
minimo (Node a Empty d) = a
minimo (Node a e d) = minimo e 

semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node a Empty d) = d
semMinimo (Node a Empty Empty) = Empty
semMinimo (Node a e d) = (Node a (semMinimo e) d)

minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin (Node a Empty d) = (a,d)
minSmin (Node a e d) =
        let (m,e') = minSmin e
        in (m, Node a e' d)

remove :: Ord a => q -> BTree a -> BTree a
remove _ Empty = Empty
remove n (Node a e d)
        | n<a = remove n e
        | n>a = remove n d
        | n==a = case (e,d) of
           (e,Empty) -> e
           (Empty, d) -> d
-- Exercício 3
type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving Show
data Classificacao = Aprov Int
                   | Rep
                   | Faltou
        deriving Show
type Turma = BTree Aluno --  arvore binária de procura (ordenada por número)

-- a 
inscNum :: Numero -> Turma -> Bool
inscNum n Empty = False
inscNum n (Node (nu, _, _, _) e d)
        | n == nu = True
        | n>nu = inscNum n d
        | n < nu = inscNum n e

inscNome :: Nome -> Turma -> Bool
inscNome n Empty = False
inscNome n (Node (_, no, _, _) e d)
        | n == no = True
        | otherwise = inscNome n e || inscNome n d

trabEst :: Turma -> [(Numero,Nome)]
trabEst (Node (Numero,_,Regime,_) e d)
        | Regime == TE = 