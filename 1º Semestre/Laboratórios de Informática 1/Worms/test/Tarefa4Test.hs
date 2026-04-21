module Main where

import Labs2025
import Tarefa4
import Magic

testesTarefa4 :: [Estado]
testesTarefa4 =
  let
    mapa1 :: Mapa
    mapa1 = replicate 9 (replicate 10 Ar) ++ [replicate 10 Pedra]

    mapa2 :: Mapa
    mapa2 = replicate 9 (replicate 10 Terra) ++ [replicate 10 Pedra]

    mapa3 :: Mapa
    mapa3 = 
        replicate 8 (replicate 10 Ar) ++ 
        [replicate 3 Pedra ++ replicate 4 Ar ++ replicate 3 Pedra] ++ 
        [replicate 3 Pedra ++ replicate 4 Agua ++ replicate 3 Pedra]

    mapa4 :: Mapa
    mapa4 = 
        let ar = replicate 10 Ar
            pilares = [Ar, Terra, Ar, Ar, Terra, Ar, Ar, Terra, Ar, Ar]
        in replicate 5 ar ++ [pilares, pilares, pilares] ++ [ar] ++ [replicate 10 Pedra]

    mapa5 :: Mapa
    mapa5 = replicate 4 (replicate 10 Pedra) ++ 
            [replicate 10 Ar, replicate 10 Ar] ++ 
            replicate 4 (replicate 10 Pedra)

    mapa6 :: Mapa
    mapa6 = replicate 4 (replicate 10 Pedra) ++ 
            [replicate 4 Ar ++ [Terra, Terra] ++ replicate 4 Ar, replicate 10 Ar] ++ 
            replicate 4 (replicate 10 Pedra)

    mapa7 :: Mapa
    mapa7 = replicate 2 (replicate 10 Ar) ++ 
            replicate 7 (take 10 (cycle [Terra, Terra, Ar, Terra, Ar])) ++ 
            [replicate 10 Pedra]

    mapa8 :: Mapa
    mapa8 = replicate 8 (replicate 10 Ar) ++ 
            [replicate 3 Ar ++ replicate 4 Terra ++ replicate 3 Ar] ++ 
            [replicate 10 Agua]

    mapa9 :: Mapa
    mapa9 = [replicate 10 Ar] ++ replicate 9 (replicate 10 Pedra)

    minhocaBase :: Minhoca
    minhocaBase = Minhoca Nothing (Viva 100) 5 5 5 5 5

    minhocaTanque = minhocaBase { vidaMinhoca = Viva 150, jetpackMinhoca = 0 }
    minhocaSniper = minhocaBase { bazucaMinhoca = 99, dinamiteMinhoca = 0, escavadoraMinhoca = 0 }
    minhocaMineira = minhocaBase { escavadoraMinhoca = 99, bazucaMinhoca = 0 }
    minhocaDesarmada = minhocaBase { jetpackMinhoca = 0, escavadoraMinhoca = 0, bazucaMinhoca = 0, minaMinhoca = 0, dinamiteMinhoca = 0 }
    minhocaCritica = minhocaBase { vidaMinhoca = Viva 1 }

    e1 = Estado mapa1 [] [minhocaBase {posicaoMinhoca = Just (8, 0)}, minhocaBase {posicaoMinhoca = Just (8, 2)}]
    e2 = Estado mapa1 [] [minhocaBase {posicaoMinhoca = Just (8, 0)}, minhocaBase {posicaoMinhoca = Just (8, 9)}]
    e3 = Estado mapa1 [] [minhocaBase {posicaoMinhoca = Just (2, 5)}, minhocaBase {posicaoMinhoca = Just (8, 5)}]
    e4 = Estado mapa1 [] [ minhocaBase {posicaoMinhoca = Just (8, 0)}, minhocaBase {posicaoMinhoca = Just (8, 1)}, minhocaBase {posicaoMinhoca = Just (8, 8)}, minhocaBase {posicaoMinhoca = Just (8, 9)}]
    e5 = Estado mapa1 [] [minhocaDesarmada {posicaoMinhoca = Just (8, 5)}, minhocaBase {posicaoMinhoca = Just (8, 4)}]
    e6 = Estado mapa2 [] [minhocaMineira {posicaoMinhoca = Just (0, 0)}, minhocaBase {posicaoMinhoca = Just (5, 5)}]
    e7 = Estado mapa2 [] [minhocaBase {posicaoMinhoca = Just (5, 2)}, minhocaBase {posicaoMinhoca = Just (5, 8)}]
    e8 = Estado mapa2 [] [minhocaDesarmada {posicaoMinhoca = Just (5, 5)}, minhocaBase {posicaoMinhoca = Just (2, 2)}]
    e9 = Estado mapa4 [] [minhocaSniper {posicaoMinhoca = Just (8, 0)}, minhocaBase {posicaoMinhoca = Just (8, 9)}]
    e10 = Estado mapa2 [] [minhocaMineira {posicaoMinhoca = Just (2, 2)}, minhocaSniper {posicaoMinhoca = Just (2, 8)}]
    e11 = Estado mapa3 [] [minhocaBase {posicaoMinhoca = Just (8, 2)}, minhocaDesarmada {posicaoMinhoca = Just (8, 3)}] 
    e12 = Estado mapa3 [] [minhocaSniper {posicaoMinhoca = Just (8, 1)}, minhocaSniper {posicaoMinhoca = Just (8, 8)}]
    e13 = Estado mapa3 [] [minhocaBase {posicaoMinhoca = Just (8, 2)}, minhocaBase {posicaoMinhoca = Just (8, 1)}]
    e14 = Estado mapa3 [] [minhocaBase {posicaoMinhoca = Just (8, 2), jetpackMinhoca = 10}, minhocaBase {posicaoMinhoca = Just (8, 7)}]
    e15 = Estado mapa3 [] [minhocaBase {posicaoMinhoca = Just (8, 0)}, minhocaBase {vidaMinhoca = Morta, posicaoMinhoca = Just (9, 5)}]
    barril = Barril {posicaoBarril = (8, 5), explodeBarril = False}
    e16 = Estado mapa1 [barril] [minhocaSniper {posicaoMinhoca = Just (8, 0)}, minhocaBase {posicaoMinhoca = Just (8, 9)}]
    e17 = Estado mapa1 [barril] [minhocaSniper {posicaoMinhoca = Just (8, 1)}, minhocaBase {posicaoMinhoca = Just (8, 6)}]
    e18 = Estado mapa1 [barril] [minhocaBase {posicaoMinhoca = Just (8, 4)}, minhocaBase {posicaoMinhoca = Just (8, 9)}]
    e19 = Estado mapa1 [Barril (8,3) False, Barril (8,6) False] [minhocaBase {posicaoMinhoca = Just (8, 0)}, minhocaBase {posicaoMinhoca = Just (8, 9)}]
    e20 = Estado mapa1 [] [minhocaBase {posicaoMinhoca = Just (8, 5)}]
    e21 = Estado mapa5 [] [minhocaSniper {posicaoMinhoca = Just (5, 0)}, minhocaSniper {posicaoMinhoca = Just (5, 9)}]
    e22 = Estado mapa6 [] [minhocaMineira {posicaoMinhoca = Just (5, 0)}, minhocaBase {posicaoMinhoca = Just (5, 9)}]
    e23 = Estado mapa5 [] [minhocaBase {posicaoMinhoca = Just (4, 5)}, minhocaBase {posicaoMinhoca = Just (5, 5)}]
    e24 = Estado mapa1 [] [ minhocaBase {posicaoMinhoca = Just (8, 4)}, minhocaBase {posicaoMinhoca = Just (8, 2)}, minhocaBase {posicaoMinhoca = Just (8, 6)}]
    e25 = Estado mapa2 [] [ minhocaBase {posicaoMinhoca = Just (1, 1)}, minhocaBase {posicaoMinhoca = Just (1, 2)}, minhocaBase {posicaoMinhoca = Just (2, 1)}, minhocaBase {posicaoMinhoca = Just (2, 2)}]
    e26 = Estado mapa1 [] [ minhocaSniper {posicaoMinhoca = Just (8, 0)}, minhocaBase {posicaoMinhoca = Just (8, 3)}, minhocaBase {posicaoMinhoca = Just (8, 8)}]
    e27 = Estado mapa1 [] [minhocaBase {posicaoMinhoca = Just (8, 0)}, minhocaCritica {posicaoMinhoca = Just (8, 5)}]
    e28 = Estado mapa1 [] [minhocaCritica {posicaoMinhoca = Just (8, 2)}, minhocaCritica {posicaoMinhoca = Just (8, 7)}]
    e29 = Estado mapa1 [] [minhocaCritica {posicaoMinhoca = Just (8, 4)}, minhocaBase {posicaoMinhoca = Just (8, 5)}]
    e30 = Estado mapa1 [] [minhocaMineira {posicaoMinhoca = Just (8, 0)}, minhocaBase {posicaoMinhoca = Just (8, 5)}]
    e31 = Estado mapa1 [] [ minhocaDesarmada {posicaoMinhoca = Just (8, 5)}, minhocaBase {posicaoMinhoca = Just (8, 0)}, minhocaBase {posicaoMinhoca = Just (8, 9)}]
    e32 = Estado mapa1 [] [minhocaBase { bazucaMinhoca = 1, escavadoraMinhoca = 0, posicaoMinhoca = Just (8, 0)}, minhocaBase {posicaoMinhoca = Just (8, 8)}]
    e33 = Estado mapa1 [] [minhocaBase {posicaoMinhoca = Just (0, 2)}, minhocaBase {posicaoMinhoca = Just (0, 7)}]
    e34 = Estado mapa1 [] [minhocaBase {posicaoMinhoca = Just (8, 5)}, minhocaBase {posicaoMinhoca = Just (1, 5), jetpackMinhoca = 10}] 
    e35 = Estado mapa8 [] [minhocaSniper {posicaoMinhoca = Just (8, 0)}, minhocaBase {posicaoMinhoca = Just (8, 5)}]
    e36 = Estado mapa7 [] [minhocaBase {posicaoMinhoca = Just (0, 0)}, minhocaBase {posicaoMinhoca = Just (5, 5)}]
    e37 = Estado mapa7 [] [minhocaMineira {posicaoMinhoca = Just (5, 5)}, minhocaBase {posicaoMinhoca = Just (0, 9)}]
    e38 = Estado mapa1 [Disparo (8,4) Norte Mina Nothing 1, Disparo (8,6) Norte Mina Nothing 1] [minhocaBase {posicaoMinhoca = Just (8, 0)}, minhocaBase {posicaoMinhoca = Just (8, 9)}]
    e39 = Estado mapa1 [Disparo (8,1) Norte Dinamite (Just 1) 1] [minhocaBase {posicaoMinhoca = Just (8, 1)}, minhocaBase {posicaoMinhoca = Just (8, 9)}]
    e40 = Estado mapa1 [Barril (8,2) False, Barril (8,3) False] [minhocaSniper {posicaoMinhoca = Just (8, 0)}, minhocaBase {posicaoMinhoca = Just (8, 4)}]
    e41 = Estado mapa1 [] [minhocaBase {posicaoMinhoca = Just (8, 0)}, minhocaBase {posicaoMinhoca = Just (8, 9)}]
    e42 = Estado mapa1 [] [minhocaBase {posicaoMinhoca = Just (0, 5)}, minhocaBase {posicaoMinhoca = Just (8, 5)}]
    e43 = Estado mapa1 [] [ minhocaBase {posicaoMinhoca = Just (8,0)}, minhocaBase {posicaoMinhoca = Just (8,9)}, minhocaBase {posicaoMinhoca = Just (8,1)}, minhocaBase {posicaoMinhoca = Just (8,8)}, minhocaBase {posicaoMinhoca = Just (8,2)}, minhocaBase {posicaoMinhoca = Just (8,7)}]
    e44 = Estado mapa1 [] [minhocaBase {posicaoMinhoca = Just (8, 5)}, minhocaBase {posicaoMinhoca = Nothing}]
    e45 = Estado mapa9 [] [minhocaBase {posicaoMinhoca = Just (0, 0)}, minhocaBase {posicaoMinhoca = Just (0, 1)}]
    e46 = Estado mapa1 [] [minhocaBase { bazucaMinhoca = 1000, escavadoraMinhoca = 1000, posicaoMinhoca = Just (8, 0)}, minhocaBase {posicaoMinhoca = Just (8, 9)}]
    e47 = Estado mapa1 [] [minhocaBase {posicaoMinhoca = Just (5, 5)}, minhocaBase {posicaoMinhoca = Just (5, 6)}]
    e48 = Estado mapa1 [] [minhocaSniper {posicaoMinhoca = Just (0, 0)}, minhocaBase {posicaoMinhoca = Just (8, 8)}]
    e49 = Estado mapa1 [] [minhocaSniper {posicaoMinhoca = Just (0, 9)}, minhocaBase {posicaoMinhoca = Just (8, 0)}]
    e50 = Estado mapa1 [] [ minhocaBase {posicaoMinhoca = Just (8, 5)}, minhocaBase {vidaMinhoca = Morta, posicaoMinhoca = Just (8, 6)}, minhocaBase {vidaMinhoca = Morta, posicaoMinhoca = Just (8, 7)}]
    
    e51 = Estado mapa1 [] [minhocaTanque {posicaoMinhoca = Just (8, 0)}, minhocaTanque {posicaoMinhoca = Just (8, 9)}]
    e52 = Estado mapa1 [] [minhocaTanque {posicaoMinhoca = Just (8, 5)}, minhocaBase {posicaoMinhoca = Just (8, 0)}, minhocaBase {posicaoMinhoca = Just (8, 9)}]
    e53 = Estado mapa1 [Barril (8, 2) False] [minhocaTanque {posicaoMinhoca = Just (8, 0)}, minhocaBase {posicaoMinhoca = Just (8, 9)}]
    e54 = Estado mapa1 [] [minhocaTanque {posicaoMinhoca = Just (8, 0)}, minhocaSniper {posicaoMinhoca = Just (8, 9)}]
    e55 = Estado mapa3 [] [minhocaTanque {posicaoMinhoca = Just (8, 2)}, minhocaBase {posicaoMinhoca = Just (8, 7)}]

  in
    [ e1, e2, e3, e4, e5, e6, e7, e8, e9, e10
    , e11, e12, e13, e14, e15, e16, e17, e18, e19, e20
    , e21, e22, e23, e24, e25, e26, e27, e28, e29, e30
    , e31, e32, e33, e34, e35, e36, e37, e38, e39, e40
    , e41, e42, e43, e44, e45, e46, e47, e48, e49, e50
    , e51, e52, e53, e54, e55
    ]

dataTarefa4 :: IO TaskData
dataTarefa4 = do
    let ins = testesTarefa4
    outs <- mapM (runTest . tatica) ins
    return $ T4 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa4