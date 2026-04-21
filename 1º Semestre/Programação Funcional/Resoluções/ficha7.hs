-- Exercício 1
data ExpInt = Const Int
    | Simetrico ExpInt
    | Mais ExpInt ExpInt
    | Menos ExpInt ExpInt
    | Mult ExpInt ExpInt deriving Show

a1 = Mais (Const 5) (Const 7)

a2 = Menos a1 (Mult (Const 6)(Const 2))
calcula :: ExpInt -> Int
calcula (Const x) = x
calcula (Simetrico e) = -1*(calcula e)
calcula (Mais e1 e2) = calcula e1 + calcula e2
calcula (Menos e1 e2) = calcula e1 - calcula e2
calcula (Mult e1 e2) = calcula e1 * calcula e2

infixa :: ExpInt -> String 
infixa (Const x) = show x
infixa (Simetrico e) = "-" ++ (infixa e)
infixa (Mais e1 e2) = "(" ++ infixa e1 ++ "+" ++ infixa e2 ++ ")"
infixa (Menos e1 e2) = "(" ++ infixa e1 ++ "-" ++ infixa e2 ++ ")"
infixa (Mult e1 e2) = "(" ++ infixa e1 ++ "*" ++ infixa e2 ++ ")"

posfixa :: ExpInt -> String 
posfixa (Const x) = show x
posfixa (Simetrico e) = (posfixa e) ++ "-"
posfixa (Mais e1 e2) = posfixa e1 ++ " " ++ posfixa e2 ++ "+"
posfixa (Menos e1 e2) = posfixa e1 ++ " " ++ posfixa e2 ++ "-"
posfixa (Mult e1 e2) = posfixa e1 ++ " " ++ posfixa e2 ++ "*"


-- Esta parte é da ficha 6
data BTree a = Empty | Node a (BTree a) (BTree a)

{-
minimo :: BTree a -> BTree a
minimo (Node x Empty d) = x
minimo (Node x e d) = minimo e

semMinimo :: BTree a -> BTree a
semMinimo (Node x Empty d) = d
semMinimo (Node x e d) = Node x (semMinimo e) d

{-
minSmin :: Btree a -> (a, Btree a)
minSmin t = (minimo t, semMinimo t)
-}

minSmin :: BTree a -> (a, BTree a)
minSmin (Node x Empty d) = (x,d)
minSmin (Node x e d) =
    let (m, e') = minSmin e
    in (m, Node x e' d)

remove :: a -> BTree a -> BTree a
remove x Empty = Empty
remove x (Node y e d)
    | x < y = Node y (remove x e) d
    | x > y = Node y e (remove d)
    | x == y = case d of 
                    Empty -> e1
                    _ -> let (m, d') = minSmin d
                         in Node m e d'

-}

-------
data RTree a = R a [RTree a ] deriving Show

r1 = R 5 []
r2 = R 3 [R 4 [], R 7 [R 2 [], R 3 [], R 6 []], R 9 [R 5 []]]

soma :: Num a => RTree a -> a 
soma (R x l) = x + sum (map soma l)

altura :: RTree a -> Int
altura (R x []) = 1
altura (R x l) = 1 + maximum (map altura l)

mirror :: RTree a -> RTree a
mirror (R x l) = R x (reverse (map mirror l))

postorder :: RTree a -> [a]
postorder (R x l) = concat (map postorder l) ++ [x]
---
data LTree a = Tip a | Fork (LTree a)(LTree a) deriving Show

lt1 = Tip 5
lt2 = Fork (Tip 3) (Fork (Tip 4) (Tip 2))

ltSoma :: Num a => LTree a -> a
ltSoma (Tip x) = x
ltSoma (Fork e d) = ltSoma e + ltSoma d

listaLT :: LTree a -> [a]
listaLT (Tip x) = [x]
listaLT (Fork e d) = listaLT e ++ listaLT d

ltHeight :: LTree a -> Int
ltHeight (Tip x) = 1
ltHeight (Fork e d) = 1 + max (ltHeight e) (ltHeight d)

data FTree a b = Leaf b | No a (FTree a b) (FTree a b) deriving Show

ft1 = No 5 (Leaf "4") (No 4 (Leaf "B")(Leaf "C"))

splitFTree :: FTree a b -> (BTree a, LTree b)
splitFTree (Leaf x) = (Empty, Tip x)
spliFTree (No y e d) = 
    let (abe, lte) = splitFTree e 
        (abd, ltd) = splitFTree d 
    in (Node y abe abd, Fork lte ltd)

joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
joinTrees Empty (Fork _ _) = Nothing
joinTrees (Node _ _ _) (Tip _) = Nothing
joinTrees Empty (Tip x) = Just (Leaf x)
joinTrees (Node x e1 d1) (Fork e2 d2) =
    let re = joinTrees e1 e2
        rd = joinTrees d1 d2
    in case re of
        Nothing -> Nothing 
        (Just e) -> case rd of 
                        Nothing -> Nothing
                        (Just d) -> Just (No x e d)