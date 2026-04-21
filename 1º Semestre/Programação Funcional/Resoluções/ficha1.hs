import Text.XHtml (base)
import Distribution.Simple.Flag (Flag)
import Data.Char(chr,ord)
perimetro :: Float -> Float
perimetro r = 2*pi*r

dist :: (Double, Double) -> (Double, Double) -> Double
dist (x1,y1) (x2, y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)

primult :: [a] -> (a, a)
primult lista = (head lista, last lista)

multiplo :: Int -> Int -> Bool
multiplo m n = mod m n == 0

truncaImpar :: [a] -> [a]
truncaImpar lista = if mod (length lista) 2 == 0 then lista else tail lista

max2 :: Int -> Int -> Int
max2 a b = if a>b then a else b

max3 :: Int -> Int -> Int -> Int
max3 a b c = max2 (max2 a b) c

nRaizes :: Float -> Float -> Float -> Int
nRaizes a b c = if d ==0 then 1 else if d>0 then 2 else 0
    where d=b^2 -4*a*c

raizes :: Float -> Float -> Float -> [Float]
raizes a b c= if nRaizes a b c ==0 then [] else if nRaizes a b c ==1 then [-b/(2*a)] else [(-b + sqrt (b^2-4*a*c))/(2*a), (-b - sqrt (b^2-4*a*c))/(2*a)]

type Hora = (Int, Int)
valida :: Hora -> Bool
valida (h,m) = h>=0 && h<24 && m >=0 && m<60
comparar :: Hora -> Hora -> Bool
comparar (h1,m1) (h2,m2) = if h1>h2 then True else if m1>m2 then True else False
horamin :: Hora -> Int
horamin (h,m) = h*60 + m
converter :: Int -> Hora
converter m = (div m 60, mod m 60)

dif :: Hora -> Hora -> Int
dif (h1,m1) (h2, m2) = (horamin (h1,m1) - horamin (h2,m2)^2)

add :: Int -> Hora -> Hora
add m (h1,m1) = (h1 + div m 60, m1 + mod m 60)
-- O Exercício 4 é só  repetir o código anterior colocando HORA1 depois de definir:
-- data HORA1 = H Int Int
-- Exercício 5 
-- Exercício 7 ficha 1
type Ponto = (Double,Double)
data Figura = Circulo Ponto Double
            | Rectangulo Ponto Ponto
            | Triangulo Ponto Ponto Ponto
            deriving (Show,Eq)
poligono :: Figura -> Bool
poligono (Circulo _ r) = r>0
poligono (Rectangulo (x1,y1) (x2,y2)) = x1/=x2 && y1/=y2
poligono (Triangulo p1 p2 p3) = 
    let d12 = dist p1 p2
        d13 = dist p1 p3
        d23 = dist p2 p3
    in (d12 < d13+d23) && (d13 < d12+d23) && (d23 < d12+d23)

vertices :: Figura -> [Ponto]
vertices (Circulo _ _) = []
vertices (Rectangulo (x1,y1) (x2,y2)) = [(x1,y1),(x2,y2),(x1,y2),(x2,y1)]
vertices (Triangulo p1 p2 p3) = [p1,p2,p3]


area :: Figura -> Double
area (Triangulo p1 p2 p3) =
    let a = dist p1 p2
        b = dist p2 p3
        c = dist p3 p1
        s = (a+b+c) / 2 -- semi-perimetro
    in sqrt (s*(s-a)*(s-b)*(s-c)) -- formula de Heron
area (Circulo c r) = pi * r^2
area (Rectangulo (x1,y1) (x2,y2)) = abs ((x1-x2)*(y1-y2))

-- Exercício 8
isLower :: Char -> Bool
isLower c = elem c ['a' .. 'z']

isDigit :: Char -> Bool
isDigit c = elem c ['0'.. '9']

isAlpha :: Char -> Bool
isAlpha c = elem c ['a'..'z'] || elem c ['A'..'Z']
-- Outra solução --
-- Posso juntar as listas com ++ --
-- isAlpha :: Char -> Bool
-- isAlpha c = elem c ['a'..'z'] ++ elem c ['A'..'Z']

toUpper :: Char -> Char
toUpper c = if isLower c then chr ((ord c) - d)
                       else c
                        where d = (ord 'a') - (ord 'A')

intToDigit :: Int -> Char
intToDigit n 
    | elem n[0..9] = chr (ord '0' + n)
    | otherwise = error "não é digito"

digitToInt :: Char -> Int
digitToInt c
    | isDigit c = ord c - ord '0'
    | otherwise = error "não é digito"

-- Outra forma de resolver sem a parte do error --
--digitToInt :: Char -> Maybe Int
--digitToInt c
 --   | isDigit c = Just (ord c - ord '0')
   -- | otherwise = Nothing

