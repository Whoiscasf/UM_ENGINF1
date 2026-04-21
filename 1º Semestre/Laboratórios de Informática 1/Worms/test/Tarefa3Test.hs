{-# OPTIONS_GHC -w #-}

module Main where

import Labs2025
import Tarefa3
import Magic

-- | Mapa largo com 12 linhas, plataformas, um lago e pedra.
mapaTesteLargo :: Mapa
mapaTesteLargo =
  [ replicate 10 Ar                               -- 0
  , replicate 10 Ar                               -- 1
  , replicate 10 Ar                               -- 2
  , replicate 10 Ar                               -- 3
  , replicate 10 Ar                               -- 4
  , [Ar,Ar,Terra,Terra,Terra,Ar,Ar,Ar,Ar,Ar]      -- 5 (Plataforma 1)
  , [Ar,Ar,Ar,Ar,Ar,Ar,Terra,Terra,Ar,Ar]          -- 6 (Plataforma 2)
  , replicate 10 Ar                               -- 7
  , replicate 10 Ar                               -- 8
  , [Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra] -- 9 (Chão)
  , [Terra,Terra,Terra,Terra,Agua,Agua,Terra,Terra,Terra,Terra] -- 10 (Chão c/ Lago)
  , [Pedra,Pedra,Pedra,Pedra,Agua,Agua,Pedra,Pedra,Pedra,Pedra] -- 11 (Base)
  ]

-- | Mapa completamente vazio.
mapaVazio :: Mapa
mapaVazio = replicate 12 (replicate 10 Ar)


viva p = Minhoca (Just p) (Viva 100) 1 1 1 1 1
vivaP d p = Minhoca (Just p) (Viva d) 1 1 1 1 1
morta p = Minhoca (Just p) Morta 1 1 1 1 1

barril p b = Barril p b

bazuca p d o = Disparo p d Bazuca Nothing o
mina p d t o = Disparo p d Mina t o
dinamite p d t o = Disparo p d Dinamite (Just t) o

--
-- Grupo 1: Gravidade Minhoca
--

-- | GM1: Minhoca no ar (8,2) pára acima do Chão (9,2).
eGM1 = Estado mapaTesteLargo [] [ viva (8,2) ]
-- | GM2: Minhoca no ar (4,2) pára acima da Plataforma 1 (5,2).
eGM2 = Estado mapaTesteLargo [] [ viva (4,2) ]
-- | GM3: Minhoca no ar (10,0) pára acima da Pedra (11,0).
eGM3 = Estado mapaTesteLargo [] [ viva (10,0) ]
-- | GM4: Minhoca cai de (8,4), passa por (9,4) (Ar) e aterra em (10,4) (Água) -> Morre.
eGM4 = Estado mapaTesteLargo [] [ viva (8,4) ]
-- | GM5: Minhoca cai para fora do mapaVazio.
eGM5 = Estado mapaVazio [] [ viva (10,5) ]
-- | GM6: Minhoca (7,2) (Ar) empilhada em Minhoca (8,2) (Ar).
--   (7,2) pára em (7,2) (batendo em (8,2)).
--   (8,2) pára em (8,2) (batendo no chão (9,2)).
eGM6 = Estado mapaTesteLargo [] [ viva (7,2), viva (8,2) ]

--
-- Grupo 2: Gravidade Barril

-- | GB1: Barril no ar (8,3) pára acima do Chão (9,3).
eGB1 = Estado mapaTesteLargo [ barril (8,3) False ] []
-- | GB2: Barril no ar (10,1) pára acima da Pedra (11,1).
eGB2 = Estado mapaTesteLargo [ barril (10,1) False ] []
-- | GB3: Barril cai de (8,5), passa por (9,5) (Ar) e aterra em (10,5) (Água) -> Ativa.
eGB3 = Estado mapaTesteLargo [ barril (8,5) False ] []
-- | GB4: Barril cai para fora do mapaVazio -> Ativa (e explode).
eGB4 = Estado mapaTesteLargo [ barril (7,3) False ] [ viva (8,3) ]
-- | GB6: Barril (7,4) (Ar) em cima de Barril (8,4) (Ar).
--   Ambos caem e param em (8,4) (acima do chão 9,4).
eGB5 = Estado mapaTesteLargo [ barril (7,4) False, barril (8,4) False ] []

--
-- Grupo 3: Bazuca (Z)
--

-- | Z1: Move-se no Ar. (7,0) E -> (7,1).
eZ1 = Estado mapaTesteLargo [ bazuca (7,0) Este 0 ] []
-- | Z2: Move-se na Água. (10,6) O -> (10,5).
eZ2 = Estado mapaTesteLargo [ bazuca (10,6) Oeste 0 ] []
-- | Z3: Bate na Borda (Sai do mapa). (7,9) E -> Explode (Timer 0).
eZ3 = Estado mapaTesteLargo [ bazuca (7,9) Este 0 ] []
-- | Z4: Bate em Terra. (8,8) E -> (8,9) (Terra). Explode (Right Danos).
eZ4 = Estado mapaTesteLargo [ bazuca (8,8) Este 0 ] []
-- | Z5: Bate em Pedra. (10,8) E -> (10,9) (Pedra). Explode.
eZ5 = Estado mapaTesteLargo [ bazuca (10,8) Este 0 ] [ viva (10,8) ] -- minhoca p/ atirar
-- | Z6: Bate em Minhoca. (8,7) E. Minhoca em (8,8). Explode.
eZ6 = Estado mapaTesteLargo [ bazuca (8,7) Este 0 ] [ viva (8,8) ]
-- | Z7: Bate em Barril. (8,6) E. Barril em (8,7). Explode. (O "famoso" Teste 28/29)
eZ7 = Estado mapaTesteLargo [ bazuca (8,6) Este 0, barril (8,7) False ] []

--
-- Grupo 4: Dinamite
--
-- | D1: No Chão (Terra). (9,1) E. Timer 3. -> Timer 2, Dir Norte, Pos (9,1).
eD1 = Estado mapaTesteLargo [ dinamite (9,1) Este 3 0 ] []
-- | D2: No Chão (Pedra). (11,2) S. Timer 3. -> Timer 2, Dir Norte, Pos (11,2).
eD2 = Estado mapaTesteLargo [ dinamite (11,2) Sul 3 0 ] []
-- | D3: No Ar (Parábola Este). (7,1) E. Timer 3. -> Timer 2, Dir Sudeste, Pos (7,2).
eD3 = Estado mapaTesteLargo [ dinamite (7,8) Oeste 3 0 ] []
-- | D5: No Ar (Queda Sul). (7,5) S. Timer 3. -> Timer 2, Dir Norte, Pos (8,5).
eD4 = Estado mapaTesteLargo [ dinamite (7,5) Sul 3 0 ] []
-- | D6: No Ar (Queda Norte). (7,6) N. Timer 3. -> Timer 2, Dir Norte, Pos (8,6).
eD5 = Estado mapaTesteLargo [ dinamite (7,6) Norte 3 0 ] []
-- | D7: Na Água. (10,4) E. Timer 3. -> Timer 2, Dir Este, Pos (10,4) (não se move).
eD6 = Estado mapaTesteLargo [ dinamite (10,4) Este 3 0 ] []
-- | D8: Timer 0 (No Ar). (7,7) E. Timer 0. -> Explode.
eD7 = Estado mapaTesteLargo [ dinamite (7,7) Este 0 0 ] []

--
-- Grupo 5: Mina
--

-- | Mi1: No Chão (Terra), Sem Timer, Sem ninguém. (9,1). -> Fica (9,1), Dir Norte.
eMi1 = Estado mapaTesteLargo [ mina (9,1) Sul Nothing 0 ] []
-- | Mi2: No Chão (Terra), Sem Timer, Owner (0) perto. (9,1). Minhoca (9,2) (ID 0). -> Fica (9,1), Dir Norte.
eMi2 = Estado mapaTesteLargo [ mina (9,1) Sul Nothing 0 ] [ viva (9,2) ]
-- | Mi3: No Chão (Terra), Sem Timer, Enemy (1) perto. (9,1). Minhoca (9,2) (ID 1). -> Fica (9,1), Dir Norte, Timer Just 2.
eMi3 = Estado mapaTesteLargo [ mina (9,1) Sul Nothing 0 ] [ viva (0,0), viva (9,2) ]
-- | Mi4: No Chão (Terra), Sem Timer, Dead Enemy (1) perto. (9,1). Minhoca Morta (9,2) (ID 1). -> Fica (9,1), Dir Norte.
eMi4 = Estado mapaTesteLargo [ mina (9,1) Sul Nothing 0 ] [ viva (0,0), morta (9,2) ]
-- | Mi5: No Chão (Terra), Com Timer 3. (9,3). -> Fica (9,3), Dir Norte, Timer Just 2.
eMi5 = Estado mapaTesteLargo [ mina (9,3) Sul (Just 3) 0 ] []
-- | Mi6: No Ar, Dir Sul. (7,5) S. -> Cai para (8,5), Dir Norte.
eMi6 = Estado mapaTesteLargo [ mina (7,5) Sul Nothing 0 ] []
-- | Mi7: No Ar, Dir Este. (7,6) E. -> Fica (7,6), Dir Este (não cai).
eMi7 = Estado mapaTesteLargo [ mina (7,6) Este Nothing 0 ] []
-- | Mi8: Na Água, Dir Sul. (10,4) S. -> Cai para (11,4) (Agua), Dir Norte.
eMi8 = Estado mapaTesteLargo [ mina (10,4) Sul Nothing 0 ] []
-- | Mi9: Na Água, Dir Este. (10,5) E. -> Fica (10,5), Dir Este (não cai).
eMi9 = Estado mapaTesteLargo [ mina (10,5) Este Nothing 0 ] []
-- | Mi10: Timer 0 (No Ar). (7,7) S. Timer 0. -> Explode.
eMi10 = Estado mapaTesteLargo [ mina (7,7) Sul (Just 0) 0 ] []

--
-- Grupo 6: Explosões
--

-- | X1: Dano em Minhoca. Barril (9,1) T. Minhoca (9,1). -> Minhoca danificada, Barril removido.
eX1 = Estado mapaTesteLargo [ barril (9,1) True ] [ viva (9,1) ]
-- | X2: Morte de Minhoca. Barril (9,2) T. Minhoca (Viva 10) (9,2). -> Minhoca Morta, Barril removido.
eX2 = Estado mapaTesteLargo [ barril (9,2) True ] [ vivaP 10 (9,2) ]
-- | X3: Destruir Terra. Barril (9,3) T. -> Mapa (9,3) vira Ar.
eX3 = Estado mapaTesteLargo [ barril (9,3) True ] []
-- | X4: Não destruir Pedra. Barril (11,1) T. -> Mapa (11,1) fica Pedra.
eX4 = Estado mapaTesteLargo [ barril (11,1) True ] []
-- | X5: Não destruir Água. Barril (10,4) T. -> Mapa (10,4) fica Água.
eX5 = Estado mapaTesteLargo [ barril (10,4) True ] []
-- | X6: Trigger Barril. Barril (9,5) T. Barril (9,6) F. -> Barril (9,5) removido, Barril (9,6) fica True.
eX6 = Estado mapaTesteLargo [ barril (9,5) True, barril (9,6) False ] []
-- | X7: Trigger Mina (com timer). Barril (9,7) T. Mina (9,8) N (Just 3) 0. -> Barril removido, Mina fica (Timer 0).
eX7 = Estado mapaTesteLargo [ barril (9,7) True, mina (9,8) Norte (Just 3) 0 ] []
-- | X8: Trigger Dinamite. Barril (9,8) T. Dinamite (9,9) N 3 0. -> Barril removido, Dinamite fica (Timer 0).
eX8 = Estado mapaTesteLargo [ barril (9,8) True, dinamite (9,9) Norte 3 0 ] []

--
-- Grupo 7: Reações em Cadeia
--

-- | C1: Barril -> Barril. (9,1) T, (9,2) F. -> Loop 1: (9,1) explode, (9,2) ativa. Loop 2: (9,2) explode.
eC1 = Estado mapaTesteLargo [ barril (9,1) True, barril (9,2) False ] []
-- | C2: Barril -> Mina. (9,1) T, Mina (9,2) N (Just 3) 0. -> Loop 1: (9,1) explode, Mina (9,2) (Timer 0). Loop 2: Mina (9,2) explode.
eC2 = Estado mapaTesteLargo [ barril (9,1) True, mina (9,2) Norte (Just 3) 0 ] []
-- | C3: Barril -> Dinamite. (9,1) T, Dinamite (9,2) N 3 0. -> Loop 1: (9,1) explode, Dinamite (9,2) (Timer 0). Loop 2: Dinamite (9,2) explode.
eC3 = Estado mapaTesteLargo [ barril (9,1) True, dinamite (9,2) Norte 3 0 ] []
-- | C4: Mina (T=1) -> Barril. Mina (9,1) N (Just 1) 0. Barril (9,2) F. -> Loop 1: Mina (9,1) explode, Barril (9,2) ativa. Loop 2: Barril (9,2) explode.
eC4 = Estado mapaTesteLargo [ mina (9,1) Norte (Just 1) 0, barril (9,2) False ] []
-- | C5: Dinamite (T=1) -> Barril. Dinamite (9,1) N 1 0. Barril (9,2) F. -> Loop 1: Dinamite (9,1) explode, Barril (9,2) ativa. Loop 2: Barril (9,2) explode.
eC5 = Estado mapaTesteLargo [ dinamite (9,1) Norte 1 0, barril (9,2) False ] []
-- | C6: Bazuca -> Barril. (Assumindo colisão eZ7). -> Loop 1: Bazuca explode (bate 8,2), Barril (8,2) ativa. Loop 2: Barril (8,2) explode.
eC6 = Estado mapaTesteLargo [ bazuca (8,1) Este 0, barril (8,2) False ] []
-- | C7: Bazuca -> Minhoca -> Barril. Bazuca (8,1) E. Minhoca (8,2). Barril (8,3) F. -> Loop 1: Bazuca explode (bate 8,2). Dano na minhoca. Barril (8,3) ativa. Loop 2: Barril (8,3) explode. Dano na minhoca (outra vez).
eC7 = Estado mapaTesteLargo [ bazuca (8,1) Este 0, barril (8,3) False ] [ viva (8,2) ]
-- | C8: Efeito "dominó" (3 barris). (9,1) T, (9,2) F, (9,3) F. -> 3 loops de explosão.
eC8 = Estado mapaTesteLargo [ barril (9,1) True, barril (9,2) False, barril (9,3) False ] []
-- | C9: Explosão simultânea. Mina (9,1) N (Just 1) 0. Dinamite (9,2) N 1 0. Barril (9,3) F.
--    Loop 1: Mina e Dinamite explodem. Barril (9,3) é ativado (pela Dinamite).
--    Loop 2: Barril (9,3) explode.
eC9 = Estado mapaTesteLargo [ mina (9,1) Norte (Just 1) 0, dinamite (9,2) Norte 1 0, barril (9,3) False ] []
-- | C10: Explosão mútua. Barril (9,1) T, Barril (9,2) T. -> Loop 1: Ambos explodem. (Não há Loop 2).
eC10 = Estado mapaTesteLargo [ barril (9,1) True, barril (9,2) True ] []


testesTarefa3 :: [Estado]
testesTarefa3 =
  [ 
    eGM1, eGM2, eGM3, eGM4, eGM5, eGM6
  , eGB1, eGB2, eGB3, eGB4, eGB5, eZ1
  , eZ3, eZ4 , eD1, eD3 , eD4 , eD5 
  , eD6, eD7, eMi1, eMi2, eMi3, eMi4
  , eMi5, eMi6, eMi7, eMi8, eMi9, eX1
  , eX3, eX4, eX5, eX6, eC1, eC4, eC5
  , eC8, eC9, eC10 
  ]

dataTarefa3 :: IO TaskData
dataTarefa3 = do
  let ins = testesTarefa3
  outs <- mapM (runTest . avancaEstado) ins
  return $ T3 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa3