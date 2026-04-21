import Data.List

data Frac = F Int Int

f1 = F 2 3

instance Show Frac where
    show (F n 1) = show n
    show (F n d ) = show n ++ "/" ++ show d

instance Eq Frac where 
    (F n1 d1) == (F n2 d2) = n1*d2 == n2*d1

-- x, y > 0
mdc :: Int -> Int -> Int
mdc x y
  | x==y = x
  | x>y = mdc (x-y) y
  | x<y = mdc x (y-x)

normaliza :: Frac -> Frac
normaliza (F n d) =
    let md = mdc (abs n) (abs d)
        n' = div (abs n) md
        d' = div (abs d) md
        sinal = if n*d <0 then (-1) else 1 
    in F (sinal*n') d'

{-
tem algum erro
instance Ord Frac where
    f1<= f2 = let (F n1 n2) = normaliza f1
                  (F n2' d2) = normaliza f2
               in n1*d2 <= n2*d1
-}


{-
Outra forma de fazer
 compare f1 f2 
     | n1*d2 == n2*d1 = EQ
     | n1*d2 < n2*d1 = LT
     | otherwise = GT
  where (F n1 d1) = normaliza f1
        (F n2 d2) = normaliza f2
-}

instance Num Frac where
    (F n1 d1) + (F n2 d2) = normaliza (F (n1*d2 + n2*d1) (d1*d2))
    (F n1 d1) * (F n2 d2) = normaliza (F (n1*n2) (d1*d2))
    (F n1 d1) - (F n2 d2) = normaliza (F (n1*d2 - n2*d1) (d1*d2))
    abs (F n d) = F (abs n) (abs d)
    signum (F n d) 
        | (n*d) == 0 = F 0 1
        | (n*d) < 0 = F (-1) 1
        | otherwise = F 1 1
    fromInteger i = (F (fromIntegral i) 1)

-- maioresDobros :: Frac -> [Frac] -> [Frac]
-- maioresDobros f l = filter (\f1 -> f1> 2*f)

data Exp a = Const a
    | Simetrico (Exp a)
    | Mais (Exp a) (Exp a)
    | Menos (Exp a) (Exp a)
    | Mult (Exp a) (Exp a)
e1 = Mais (Const 5) (Const 3)
e2 = Mult (Const (F 2 3)) (Const 5)

infixa :: Show a => Exp a -> String 
infixa (Const x) = show x
infixa (Simetrico e) = "-" ++ (infixa e)
infixa (Mais e1 e2) = "(" ++ infixa e1 ++ "+" ++ infixa e2 ++ ")"
infixa (Menos e1 e2) = "(" ++ infixa e1 ++ "-" ++ infixa e2 ++ ")"
infixa (Mult e1 e2) = "(" ++ infixa e1 ++ "*" ++ infixa e2 ++ ")"

instance Show a => Show (Exp a) where
    show e = infixa e

calcula :: Num a => (Exp a) -> a
calcula (Const x) = x
calcula (Simetrico e) = -1 * calcula e
calcula (Mais e1 e2) = calcula e1 + calcula e2
calcula (Menos e1 e2) = calcula e1 - calcula e2
calcula (Mult e1 e2) = calcula e1 * calcula e2

instance (Num a, Eq a) => Eq (Exp a) where
    e1 == e2 = calcula e1 == calcula e2

instance (Num a, Eq a) => Num (Exp a) where
    e1 + e2 = Const (calcula e1 + calcula e2)
    e1 - e2 = Const (calcula e1 - calcula e2)
    e1 * e2 = Const (calcula e1 * calcula e2)
    signum e = Const (signum (calcula e))
    abs e = Const (abs (calcula e))
    fromInteger i = Const (fromInteger i)

-- Exercício 3
data Movimento = Credito Float | Debito Float
data Data = D Int Int Int -- Dia, mes, ano
data Extracto = Ext Float [(Data, String, Movimento)]

lm = [(D 1 1 2000, "AAA", Credito 10), (D 1 2 1999, "BBB", Debito 5), (D 1 5 2000, "CCC", Credito 20)]
ex = Ext 1000 lm
instance Eq Data where
    (D d1 m1 a1) == (D d2 m2 a2) = (d1,m1,a1)==(d2,m2,a2)
 
instance Ord Data where
    (D d1 m1 a1) <= (D d2 m2 a2) = (a1,m1,d1)<=(a2,m2,d2)

instance Show Data where
    show (D d m a) = show a ++ "/" ++ show m ++ "/" ++ show d

ordena :: Extracto -> Extracto
ordena (Ext si lm) = Ext si (sortOn (\(d,_,_) -> d) lm)

ordena' :: Extracto -> Extracto
ordena' (Ext si lm) = Ext si lm'
    where lm' = sortBy compara lm
          compara :: (Data, String, Movimento) -> (Data, String, Movimento) -> Ordering
          compara (d1, _, _) (d2,_,_)
             | d1<d2 = LT
             | d1==d2 = EQ
             | d1=d2 = GT



