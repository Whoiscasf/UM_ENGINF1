import System.Random
import Data.List

{-


bingo :: IO ()
bingo = bingoaux [] 10
        where bingoaux :: [Int] -> Int -> IO ()
              bingoaux l 0 = l putStrLn $ unlines $ map show l
              bingoaux l n = do 
                              k <- randomRIO (1,10)
                              if elem k l then bingoaux l n
                               else bingoaux (k:l) (n-1)
-}
{--
geraSequencia :: IO [Int]
geraSequencia = sequence $ [ randomRIO (0,9) | x <- [1..4] ]
                 putStrLn (show l)
--}
geraSequencia :: IO [Int]
geraSequencia = sequence $ [ randomRIO (0,9) | x <- [1..4] ]

leSequencia :: IO [Int]
leSequencia = do ls <- sequence $ [getLine | x <- [1..4] ]
                 return (map read ls)

masterMind :: IO ()
masterMind = do lg <- geraSequencia
                putStrLn (show lg)
                putStrLn "Insira valores"
                lj <- leSequencia
                let lp = zip lj lg
                    lpi = filter (\(x,y)-> x==y) lp
                    lpd = filter (\(x,y)->x/=y) lp
                    (ldg, ldj) = unzip lpd
                    n= aux ldg ldj
                putStrLn $ "Numeros certos nas posições certas:" ++ show (length lpi)
                putStrLn $ "Numeros certos ans posições erradas" ++ show n

aux [] _ = 0
aux _ [] = 0
aux ldg (x:xs)
  | elem x ldg = 1+ aux (delete x ldg) xs
  | otherwise = aux ldg xs