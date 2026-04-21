module Main where

import Labs2025
import Tarefa2
import Magic

mapa :: Mapa
mapa =
    [ [Ar, Ar, Ar, Ar, Ar, Ar]
    , [Ar, Ar, Pedra, Ar, Ar, Ar]
    , [Ar, Ar, Ar, Ar, Agua, Ar]
    , [Ar, Ar, Ar, Ar, Ar, Ar]
    , [Terra, Terra, Ar, Terra, Terra, Terra]
    , [Terra, Terra, Terra, Terra, Terra, Terra]
    ]

-- Definições de Minhocas
m1, m2, m3, m4, m5 :: Minhoca
m1 = Minhoca (Just (3,1)) (Viva 100) 10 10 10 10 10
m2 = Minhoca (Just (1,1)) (Viva 100) 10 10 10 10 10
m3 = Minhoca (Just (4,3)) (Viva 100) 10 10 10 10 10
m4 = Minhoca (Just (3,2)) (Viva 100) 10 10 10 10 10
m5 = Minhoca (Just (1,4)) (Viva 100) 10 10 10 10 10

-- Variações para testes de limite
m6, m7, m8, m9, m10, m11 :: Minhoca
m6  = m1 { posicaoMinhoca = Just (0,2) }
m7  = m1 { posicaoMinhoca = Just (3,0) }
m8  = m1 { posicaoMinhoca = Just (2,3) }
m9  = m1 { posicaoMinhoca = Just (3,3) }
m10 = m1 { posicaoMinhoca = Just (5,2) }
m11 = m1 { posicaoMinhoca = Just (3,5) }

-- Minhocas inválidas/mortas
mDead, mNoPos, mEmpty :: Minhoca
mDead  = m1 { vidaMinhoca = Morta, posicaoMinhoca = Just (3,3) }
mNoPos = m1 { vidaMinhoca = Morta, posicaoMinhoca = Nothing }
mEmpty = m1 { jetpackMinhoca=0, escavadoraMinhoca=0, bazucaMinhoca=0, minaMinhoca=0, dinamiteMinhoca=0 }

-- Objetos
b1, b2, d1 :: Objeto
b1 = Barril (3,4) False
b2 = Barril (5,3) False
d1 = Disparo (1,0) Este Bazuca Nothing 0

-- Estados
s1, s2, s3, s4, s5, s6, s7, s8, s9 :: Estado
s1 = Estado mapa [b1] [m1, m2, m3]
s2 = Estado mapa []   [m4, m1]
s3 = Estado mapa []   [m5, m1]
s4 = Estado mapa []   [mEmpty, mDead, mNoPos]
s5 = Estado mapa [d1] [m1, m2]
s6 = Estado mapa [b1] [m9, m2, m3]
s7 = Estado mapa [b2] [m1, m2, m3]
s8 = Estado mapa []   [m6, m7, m8]
s9 = Estado mapa []   [m10, m11]

type Teste = (Int, Jogada, Estado)

testes :: [Teste]
testes =
    [ (1, Move Norte, s4)
    , (2, Move Sul, s4)
    , (0, Move Norte, s2)
    , (0, Move Sul, s2)
    , (0, Move Este, s2)
    , (0, Move Norte, s3)
    , (0, Move Sul, s3)
    , (0, Move Oeste, s3)
    , (0, Move Norte, s1)
    , (0, Move Este, s1)
    , (0, Move Sudeste, s1)
    , (1, Move Este, s1)
    , (0, Move Sul, s1)
    , (2, Move Sudoeste, s1)
    , (2, Move Norte, s1)
    , (0, Move Este, s6)
    , (0, Move Sul, s6)
    , (0, Move Norte, s8)
    , (1, Move Oeste, s8)
    , (2, Move Este, s8)
    , (1, Move Sudoeste, s1)
    , (2, Move Este, s1)
    , (0, Move Nordeste, s1)
    , (1, Move Noroeste, s1)
    , (2, Move Sudeste, s1)
    , (0, Move Sudoeste, s1)
    , (1, Move Sudeste, s1)
    , (2, Move Noroeste, s1)
    , (0, Move Oeste, s1)
    , (1, Move Sul, s1)
    , (2, Move Oeste, s1)
    , (0, Move Noroeste, s1)
    , (1, Move Norte, s1)
    , (2, Move Sul, s1)
    , (0, Move Sudoeste, s1)
    , (1, Move Nordeste, s1)
    , (2, Move Sudoeste, s1)
    , (0, Move Sul, s6)
    , (0, Move Norte, s6)
    , (1, Move Nordeste, s6)
    , (1, Move Este, s6)
    , (0, Dispara Bazuca Norte, s4)
    , (1, Dispara Bazuca Norte, s4)
    , (2, Dispara Bazuca Norte, s4)
    , (0, Dispara Bazuca Norte, s5)
    , (0, Dispara Jetpack Norte, s1)
    , (1, Dispara Jetpack Este, s1)
    , (0, Dispara Jetpack Sul, s1)
    , (0, Dispara Jetpack Norte, s8)
    , (2, Dispara Jetpack Este, s8)
    , (2, Dispara Escavadora Sul, s1)
    , (2, Dispara Escavadora Sudoeste, s1)
    , (1, Dispara Escavadora Este, s1)
    , (0, Dispara Escavadora Norte, s1)
    , (2, Dispara Escavadora Este, s8)
    , (0, Dispara Escavadora Norte, s8)
    , (2, Dispara Escavadora Oeste, s1)
    , (2, Dispara Escavadora Sul, s7)
    , (0, Dispara Bazuca Este, s1)
    , (1, Dispara Bazuca Este, s1)
    , (0, Dispara Bazuca Norte, s8)
    , (0, Dispara Bazuca Este, s6)
    , (0, Dispara Mina Este, s1)
    , (1, Dispara Mina Este, s1)
    , (0, Dispara Mina Este, s6)
    , (0, Dispara Mina Norte, s8)
    , (0, Dispara Dinamite Este, s1)
    , (1, Dispara Dinamite Este, s1)
    , (0, Dispara Dinamite Este, s6)
    , (0, Dispara Dinamite Norte, s8)
    , (1, Dispara Bazuca Sul, s5)
    , (0, Dispara Mina Sul, s5)
    , (0, Dispara Escavadora Sul, s6)
    , (1, Dispara Jetpack Nordeste, s1)
    , (2, Dispara Escavadora Norte, s1)
    , (0, Dispara Bazuca Sudoeste, s1)
    , (1, Dispara Mina Sul, s1)
    , (2, Dispara Dinamite Oeste, s1)
    , (0, Dispara Jetpack Sudoeste, s1)
    , (2, Dispara Bazuca Norte, s1)
    , (0, Dispara Mina Sudoeste, s1)
    , (0, Move Nordeste, s3)
    , (0, Move Noroeste, s3)
    , (0, Move Sudeste, s3)
    , (0, Move Sudoeste, s3)
    , (0, Move Noroeste, s2)
    , (0, Move Nordeste, s2)
    , (0, Move Sudeste, s2)
    , (0, Move Sudoeste, s2)
    , (99, Move Norte, s1)
    , (0, Dispara Jetpack Este, s6)
    , (0, Dispara Jetpack Sul, s6)
    , (0, Dispara Escavadora Este, s6)
    , (0, Dispara Mina Sul, s6)
    , (0, Dispara Dinamite Sul, s6)
    , (0, Dispara Dinamite Sul, s1)
    , (0, Dispara Bazuca Sul, s1)
    , (0, Dispara Bazuca Oeste, s1)
    , (0, Dispara Bazuca Sudeste, s1)
    , (0, Dispara Bazuca Nordeste, s1)
    , (0, Dispara Bazuca Noroeste, s1)
    , (99, Dispara Bazuca Norte, s1)
    , (1, Move Este, s9)
    , (1, Move Nordeste, s9)
    , (1, Move Sudeste, s9)
    , (0, Move Nordeste, s8)
    , (0, Move Noroeste, s8)
    , (1, Move Noroeste, s8)
    , (1, Move Sudoeste, s8)
    , (0, Dispara Jetpack Sul, s9)
    , (1, Dispara Escavadora Este, s9)
    , (0, Dispara Bazuca Sudeste, s9)
    , (1, Dispara Mina Este, s9)
    , (0, Dispara Dinamite Sul, s9)
    , (1, Dispara Jetpack Nordeste, s9)
    ]

dataTarefa2 :: IO TaskData
dataTarefa2 = do
    let ins = testes
    outs <- mapM (\(i,j,e) -> runTest $ efetuaJogada i j e) ins
    return $ T2 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa2