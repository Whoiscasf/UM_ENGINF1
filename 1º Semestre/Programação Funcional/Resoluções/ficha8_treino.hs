data Frac = F Integer Integer

normaliza :: Frac -> Frac
normaliza (F x y) = F (div a c) (div b c)
                 where a = abs x 
                       b = abs y
                       c = mdc a b

mdc :: Integer -> Integer -> Integer
mdc x 0 = x
mdc x y = mdc y (mod x y)

instance Eq Frac where
    (F n1 d1) == (F n2 d2) = n1*d2==d1*n2

instance Ord Frac where
    (F n1 d1) <= (F n2 d2) = n1 * d2 <= d1*d2

instance Show Frac where
    show (F n1 d1) = show n1 ++ "/" ++ show d1

instance Num Frac where
    (F n1 d1) + (F n2 d2) = normaliza (F (a+b) b)
                          where a = n1*d2
                                b = d1*d2
                                c = n2*d1
    (F n1 d1) - (F n2 d2) = normaliza (F (a-b) b)
                          where a = n1*d2
                                b = d1*d2
                                c = n2*d1
    (F n1 d1) * (F n2 d2) = normaliza (F (n1*n2) (d1*d2))
    negate (F n1 d1) = (F (-n1) d1)
    abs (F n1 d1) = (F (abs n1) (abs d1))
    signum (F n1 d1)
        | n1 == 0 = 0
        | div n1 d1 < 0 = -1
        | div n1 d1 > 0 = 1
    fromInteger x = (F x 1)

seleciona :: Frac -> [Frac] -> [Frac]
seleciona f [] = []
seleciona (F n d) ((F n2 d2):xs) 
    | (F (2*n) (d)) < (F n2 d2) = (F n2 d2) :seleciona (F n d) xs
    | otherwise = seleciona (F n d) xs

data Exp a = Const a
    | Simetrico (Exp a)
    | Mais (Exp a) (Exp a)
    | Menos (Exp a) (Exp a)
    | Mult (Exp a) (Exp a)

instance (Show a) => Show (Exp a) where
    show (Const a) = show a
    show (Simetrico a) = "(-" ++ show a ++ ")"
    show (Mais a b) = "(" ++ show a ++ "+" ++ show b ++ ")"
    show (Menos a b) = "(" ++ show a ++ "-" ++ show b ++ ")"
    show (Mult a b) = "(" ++ show a ++ "*" ++ show b ++ ")"

instance (Eq a) => Eq (Exp a) where
    Const a == Const b = a==b
    Simetrico a == Simetrico b = a==b
    Mais a b == Mais c d = a==c && b==d
    Menos a b == Menos c d = a==c && b==d
    Mult a b == Mais c d = a==c && b==d

instance (Num a) => Num (Exp a) where
    a + b = Mais a b
    a - b = Menos a b 
    a * b = Mult a b 
    negate a = Simetrico a

data Movimento = Credito Float | Debito Float
data Data = D Int Int Int
data Extracto = Ext Float [(Data, String, Movimento)]

instance Eq Data where
    D d1 m1 a1 == D d2 m2 a2 = d1 == d2 && m1 == m2 && a1 == a2

instance Ord Data where
    D d1 m1 a1 <= D d2 m2 a2 = a1 <= a2 ||  m1 <= m2 && a1==a2 || d1 <= d2 && m1 == m2 && a1 == a2






