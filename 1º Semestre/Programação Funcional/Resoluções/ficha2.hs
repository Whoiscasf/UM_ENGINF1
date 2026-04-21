import Data.Char

dobros :: [Float] -> [Float]
dobros (h:t) = (2*h) : dobros t
dobros [] = []

numocorre :: Char->String->Int
numocorre c [] = 0
numocorre c (h:t) = if c==h then 1 + numocorre c t else numocorre c t

positivos:: [Int]->Bool
positivos [] = True
positivos (h:t) = if h>0 then positivos t else False

sopos :: [Int] -> [Int]
sopos [] = []
sopos (h:t) = if h>=0 then h : sopos t else sopos t

somaneg :: [Int] -> Int
somaneg [] = 0
somaneg (h:t) = if h<=0 then h + somaneg t else somaneg t

tresult :: [a] -> [a]
tresult [] = []
tresult (a) = if length (a)<3 then (a) else [((a) !! (length (a)-3)), ((a) !! (length (a)-2)), (last (a))]

-- outra forma de resolver
-- tresult :: [a] -> [a]
-- tresult [] = []
-- tresult (h:t) = if length (h:t) <= 3 then (h:t) else tresult t
segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((a,b):t) = b :segundos t

{-
nosprimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosprimeiros a [] = False
nosprimeiros a ((x,y):t) 
 | a==x = True
 | x/= nosprimeiros a t

-}


-- nosprimeiros :: (Eq a) => a -> [(a,b)] -> Bool
-- nosprimeiros a [] = False
-- nosprimeiros a ((x,y):t) = x==y || nosprimeiros a t

sumtriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumtriplos [] = (0,0,0)
sumtriplos ((a,b,c):t) = let (x,y,z) = sumtriplos t in (a+x, b+y, c+z)

{- outra forma de resolver
somaprimeiros :: num a => [(a,b)] -> a
somaprimeiros [] = 0
somaprimeiros (h:t) = fst h + somaprimeiros t
somasegundos num b => [(a,b)] -> b
somasegundos [] = 0
somasegundos ((x,y):t) = y + somasegundos t
soma :: num a num b => [(a,b)] -> (a,b)
soma l = (somaprimeira l, somasegunda l)
-}




-- exercício 3 --
sodigitos :: [Char] -> [Char]
sodigitos [] = []
sodigitos (h:t) = 
    if ord h>=48 && ord h <=57 
        then h : sodigitos t 
        else sodigitos t

-- o mesmo exercicio mas com o isDigit que está definida em data.Char
-- sodigitos :: [Char] -> [Char]
-- sodigitos [] = []
-- sodigitos (h:t)
--          | isDigit h = h : sodigitos t
--          | otherwise = sodigitos t

minusculas :: [Char] -> Int
minusculas [] = 0
minusculas (h:t) = if ord h>=97 && ord h<=122 then 1+ minusculas t else minusculas t

nums :: String -> [Int]
nums [] = []
nums (h:t) = if elem h ['0'..'9'] then (ord h - ord '0') : nums t else nums t

-- O mesmo exercíco  a utilizar a função digitToInt do Data.Char
-- nums :: String -> [Int]
-- nums [] = []
-- nums (h:t) 
--    | isDigit h = digitToInt h : nums t
--      | otherwise = nums t

-- exercício 4 --
type Polinomio = [Monomio]
type Monomio = (Float, Int)

conta :: Int -> Polinomio -> Int
conta g [] = 0
conta g ((c,e):t)
    | g==e =1 + conta g t
    | otherwise = conta g t

grau :: Polinomio -> Int
grau [(c,e)] = e
grau ((c,e):t) = if e> grau t then e else grau t

--grau :: Polinomio -> Int
--grau [(c,e)] = e
--grau ((c,e):t) = max (grau t) e

-- grau :: Polinomio -> Int
-- grau [(c,e)] = e
-- grau ((c,e):(c1,e1):t)
--   | e<e1 = grau ((c1,e1):t)
--   | otherwise = grau ((c,e):t)

selgrau :: Int -> Polinomio -> Polinomio
selgrau g [] =[]
selgrau g ((c,e):t)
    | g==e = (c,e) :selgrau g t
    | otherwise = selgrau g t

deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((c,e):t)
    | e /=0 = (c*(fromIntegral e), e-1) :deriv t
    | otherwise = deriv t

calcula :: Float -> Polinomio -> Float
calcula _ [] = 0
calcula f ((c,e):t) = (f^e)*c + calcula f t

simp :: Polinomio -> Polinomio
simp [] = []
simp ((c,e):t) 
    | e==0 = simp t
    | otherwise = (c,e) : simp t

mult :: Monomio -> Polinomio -> Polinomio
mult (c,e) [] = []
mult (c,e) ((c1,e1):t) = (c*c1,e+e1) :mult (c,e) t

--normaliza :: Polinomio -> Polinomio
--normaliza 

ordena :: Polinomio -> Polinomio
ordena [] = []
ordena (h:t) = insere h (ordena t)
    where insere h [] = [h]
          insere (c,e) ((c1,e1):t)
            | e<=e1 = (c,e) : (c1,e1) : t
            | e>e1 = (c1,e1) :insere (c,e) t

maiores :: Monomio -> Polinomio -> Polinomio
maiores x [] = []
maiores (c,e) ((c1,e1):ys) 
            | e>=e1 = maiores (c,e) ys
            | e<e1 = (c1,e1) :maiores (c,e) ys

menores :: Monomio -> Polinomio -> Polinomio
menores x [] = []
menores (c,e) ((c1,e1):ys)
        | e<=e1 = menores (c,e) ys
        | e>e1 = (c1,e1) :menores (c,e) ys

ordenaQS :: Polinomio -> Polinomio
ordenaQS [] = []
ordenaQS (x:xs) = let lmai = maiores x xs
                      lmen = menores x xs
                  in ordenaQS lmen ++ [x] ++ ordenaQS lmai

parteQS :: Monomio -> Polinomio -> (Polinomio,Polinomio)
parteQS _ [] = ([],[])
parteQS (c,e) ((c1,e1):rp) =
    let (lmen,lmai) = parteQS (c,e) rp
    in if e1<=e then ((c1,e1):lmen, lmai)
                else (lmen, (c1,e1):lmai)

normaliza :: Polinomio -> Polinomio
normaliza p = auxNormaliza (ordena p)

auxNormaliza [] = []
auxNormaliza [x] = [x]
auxNormaliza ((c1,e1):(c2,e2):rp)
        | e1==e2 = auxNormaliza ((c1+c2,e1):rp)
        | e1/=e2 = if c1/=0 then (c1,e1) :auxNormaliza ((c2,e2):rp)
                            else auxNormaliza ((c2,e2):rp)

soma :: Polinomio -> Polinomio -> Polinomio
soma p1 p2 = normaliza(p1++p2)

-- a proxima função não tem nada a ver com a ficha 

merge :: Ord a => [a] -> [a] -> [a]
merge [] l = l
merge l [] = l
merge (x:xs) (y:ys)
    | x<y = x :merge xs (y:ys)
    | x>=y = y :merge (x:xs) ys

meio :: [a] -> ([a],[a])
meio [] = ([],[])
meio [x] = ([x],[])
meio (x:y:t) = (x:le,y:ld)
    where (le,ld) = meio t