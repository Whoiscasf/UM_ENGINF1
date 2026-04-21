import Data.Char

digitAlpha :: String -> (String,String)
digitAlpha [] = ("","")
digitAlpha (h:t) 
    | isDigit h = (h:ld,ll)
    | isAlpha h = (ld, h:ll)
    | otherwise = (ld,ll)
        where (ld, ll) = digitAlpha t

nzp :: [Int] -> (Int,Int,Int)
nzp [] = (0,0,0)
nzp (h:t)
    | h>0 = (n,z,1+p)
    | h<0 = (1+n, z, p)
    | h==0 = (n,1+z, p)
        where (n,z,p) = nzp t
divMod' :: Int -> Int -> (Int,Int)
divMod' x y
    | x<y = (0,x)
    | x>= y = (q+1, r)
        where (q,r) = divMod' (x-y) y 

fromDigits :: [Int] -> Int
fromDigits l = aux 0 l
    where aux n [] = n
          aux n (x:xs) = aux (x+n*10) xs

maxSumInit :: [Int] -> Int
maxSumInit [] = 0
maxSumInit (h:t) = acc h h t
    where acc m s [] = m --'m' é o valor do máximo da soma e 's' o valor da soma
          acc m s (x:xs) 
            | s+x>m = acc (s+x) (s+x) xs
            | otherwise = acc m (s+x) xs

fib :: Int -> Int
fib 0 = 0
fib n = aux 0 1 n 
    where aux n1 n2 1 = n2
          aux n1 n2 n = aux n2 (n1+n2) (n-1)

intToStr :: Int -> String
intToStr n = aux "" n
    where aux s 0 = s
          aux s n = let (q,r) = divMod' n 10
                    in aux (intToDigit r :s) q

