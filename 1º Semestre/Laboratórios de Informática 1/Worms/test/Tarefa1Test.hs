module Main where

import Labs2025
import Tarefa1
import Magic

mapa :: Mapa
mapa =
  [ [Ar, Ar, Ar, Ar, Pedra]       
  , [Ar, Terra, Ar, Agua, Ar]      
  , [Ar, Ar, Ar, Ar, Ar]          
  , [Terra, Ar, Agua, Ar, Ar]      
  , [Pedra, Ar, Ar, Ar, Ar]        
  ]

mapaUmaCelula :: Mapa
mapaUmaCelula = [[Ar]]

mapaUmaLinha :: Mapa
mapaUmaLinha  = [[Ar, Terra, Agua, Pedra]]

mapaUmaColuna :: Mapa
mapaUmaColuna = [[Ar], [Terra], [Agua], [Pedra]]


m_valida_alt1 :: Minhoca
m_valida_alt1 = Minhoca (Just (0,0)) (Viva 100) 1 1 1 1 1

m_valida_alt2 :: Minhoca
m_valida_alt2 = Minhoca (Just (1,2)) (Viva 50) 0 0 0 0 0

m_valida_morta_alt :: Minhoca
m_valida_morta_alt = Minhoca (Just (1,3)) Morta 1 1 1 1 1

b_valido_alt1 :: Objeto
b_valido_alt1 = Barril (2,2) False

d_valido_alt1 :: Objeto
d_valido_alt1 = Disparo (2,3) Norte Mina (Just 3) 0

m_base_alt :: [Minhoca]
m_base_alt = [m_valida_alt1, m_valida_alt2]


-- 1–3: validaMapa
ex1 :: Estado
ex1  = Estado mapaUmaCelula [] []             

ex2 :: Estado
ex2  = Estado mapaUmaLinha [] []               

ex3 :: Estado
ex3  = Estado mapaUmaColuna [] []              

-- 4–18: validaMinhocas
ex4 :: Estado
ex4  = Estado mapa [] [m_valida_alt1, Minhoca (Just (5,5)) (Viva 50) 1 1 1 1 1]

ex5 :: Estado
ex5  = Estado mapa [] [m_valida_alt1, Minhoca (Just (0,5)) (Viva 50) 1 1 1 1 1]

ex6 :: Estado
ex6  = Estado mapa [] [m_valida_alt1, Minhoca (Just (1,1)) (Viva 50) 1 1 1 1 1]

ex7 :: Estado
ex7  = Estado mapa [] [m_valida_alt1, Minhoca (Just (4,0)) (Viva 50) 1 1 1 1 1]

ex8 :: Estado
ex8  = Estado mapa [] [m_valida_alt1, m_valida_alt1]                   -- sobreposição

ex9 :: Estado
ex9  = Estado mapa [b_valido_alt1] [m_valida_alt1, Minhoca (Just (2,2)) (Viva 50) 1 1 1 1 1]

ex10 :: Estado
ex10 = Estado mapa [] [m_valida_alt1, Minhoca (Just (1,3)) (Viva 50) 1 1 1 1 1]

ex11 :: Estado
ex11 = Estado mapa [] [m_valida_alt1, Minhoca (Just (3,2)) (Viva 50) 1 1 1 1 1]

ex12 :: Estado
ex12 = Estado mapa [] [m_valida_alt1 {posicaoMinhoca = Nothing, vidaMinhoca = Viva 50}]

ex13 :: Estado
ex13 = Estado mapa [] [m_valida_alt1 {vidaMinhoca = Viva 101}]

ex14 :: Estado
ex14 = Estado mapa [] [m_valida_alt1 {vidaMinhoca = Viva (-1)}]

ex15 :: Estado
ex15 = Estado mapa [] [m_valida_alt1 {jetpackMinhoca = -1}]

ex16 :: Estado
ex16 = Estado mapa [] [m_valida_alt1 {escavadoraMinhoca = -1}]

ex17 :: Estado
ex17 = Estado mapa [] [m_valida_alt1 {bazucaMinhoca = -1}]

ex18 :: Estado
ex18 = Estado mapa [] [m_valida_alt1 {minaMinhoca = -1}]

-- 19–23: validaObjetos – Barris
ex19 :: Estado
ex19 = Estado mapa [Barril (5,5) False] m_base_alt

ex20 :: Estado
ex20 = Estado mapa [Barril (0,4) False] m_base_alt

ex21 :: Estado
ex21 = Estado mapa [Barril (3,0) False] m_base_alt

ex22 :: Estado
ex22 = Estado mapa [b_valido_alt1, Barril (2,2) False] m_base_alt

ex23 :: Estado
ex23 = Estado mapa [Barril (0,0) False] m_base_alt

-- 24–37: validaObjetos – Disparos
ex24 :: Estado
ex24 = Estado mapa [Disparo (0,1) Norte Jetpack Nothing 0] m_base_alt

ex25 :: Estado
ex25 = Estado mapa [Disparo (0,1) Norte Escavadora Nothing 0] m_base_alt

ex26 :: Estado
ex26 = Estado mapa [Disparo (0,1) Norte Mina (Just 1) (-1)] m_base_alt

ex27 :: Estado
ex27 = Estado mapa [Disparo (0,1) Norte Mina (Just 1) 2] m_base_alt

ex28 :: Estado
ex28 = Estado mapa [Disparo (0,1) Norte Mina (Just 1) 0] []             -- dono inválido

ex29 :: Estado
ex29 = Estado mapa [Disparo (5,5) Norte Mina (Just 1) 0] m_base_alt     -- fora do mapa

ex30 :: Estado
ex30 = Estado mapa [Disparo (1,1) Norte Mina (Just 1) 0] m_base_alt     -- em Terra

ex31 :: Estado
ex31 = Estado mapa [Disparo (1,3) Norte Mina (Just 1) 0] m_base_alt     -- em Água

ex32 :: Estado
ex32 = Estado mapa [Disparo (3,2) Norte Dinamite (Just 1) 0] m_base_alt -- em Água

ex33 :: Estado
ex33 = Estado mapa [b_valido_alt1, Disparo (2,2) Norte Mina (Just 1) 0] m_base_alt

ex34 :: Estado
ex34 = Estado mapa [b_valido_alt1, Disparo (2,2) Norte Dinamite (Just 1) 0] m_base_alt

ex35 :: Estado
ex35 = Estado mapa [Disparo (0,1) Norte Mina (Just (-1)) 0] m_base_alt

ex36 :: Estado
ex36 = Estado mapa [Disparo (0,1) Norte Dinamite Nothing 0] m_base_alt

ex37 :: Estado
ex37 = Estado mapa [Disparo (0,1) Norte Bazuca (Just 1) 0] m_base_alt

-- 38–40: Exceção Bazuca
ex38 :: Estado
ex38 = Estado mapa [Disparo (0,4) Este Bazuca Nothing 0] m_base_alt    -- válido

ex39 :: Estado
ex39 = Estado mapa [Disparo (3,0) Oeste Bazuca Nothing 0] m_base_alt   -- inválido

ex40 :: Estado
ex40 = Estado mapa [Disparo (4,0) Sul Bazuca Nothing 0] m_base_alt     -- inválido



testesTarefa1 :: [Estado]
testesTarefa1 =
  [ ex1, ex2, ex3, ex4, ex5, ex6, ex7, ex8, ex9, ex10
  , ex11, ex12, ex13, ex14, ex15, ex16, ex17, ex18, ex19, ex20
  , ex21, ex22, ex23, ex24, ex25, ex26, ex27, ex28, ex29, ex30
  , ex31, ex2, ex33, ex34, ex35, ex36, ex37, ex38, ex39, ex40
  ]

dataTarefa1 :: IO TaskData
dataTarefa1 = do
  let ins = testesTarefa1
  outs <- mapM (runTest . validaEstado) ins
  return $ T1 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa1