module Mapas where

import Labs2025
import Worms



larguraMapa, alturaMapa :: Int
larguraMapa = 30
alturaMapa  = 20

heightmap :: [Int]
heightmap =
  [12,12,12,11,11,10,10,10,11,11
  ,12,13,14,14,13,12,11,10,9,9
  ,9,10,11,12,12,11,10,10,11,12
  ]

heightmap2 :: [Int]
heightmap2 =
  [12,12,11,11,10,10,9,9,10,11
  ,12,12,12,11,10,9,8,8,9,10
  ,11,12,13,13,12,11,10,10,11,12
  ]

heightmap3 :: [Int]
heightmap3 =
  [12,12,11,11,10,10,9,9,10,11
  ,12,13,14,14,13,12,11,10,9,9
  ,10,11,12,12,11,10,10,11,12,12
  ]


terrenoEm :: Int -> Int -> Terreno
terrenoEm l c
  | c < 0 || c >= larguraMapa = Ar
  | l < 0 || l >= alturaMapa  = Ar
  | l == alturaMapa - 1 = Agua
  | c >= 14 && c <= 18 && l >= 13 = Agua
  | l >= alturaMapa - 3 = Pedra
  | c == 6 && l >= 6 && l <= 15 && not (l >= 10 && l <= 12) = Pedra
  | l == 8 && c >= 12 && c <= 20 = Pedra
  | l >= 10 && l <= 12 && c >= 22 && c <= 25 = Pedra
  | l >= 11 && l <= 13 && c >= 2 && c <= 5 = Ar
  | l >= heightmap !! c = Terra
  | otherwise = Ar

terrenoEm2 :: Int -> Int -> Terreno
terrenoEm2 l c
  | c < 0 || c >= larguraMapa = Ar
  | l < 0 || l >= alturaMapa  = Ar
  | l == alturaMapa - 1       = Agua
  | l >= alturaMapa - 3       = Pedra
  | c >= 5 && c <= 8 && l >= 12 = Agua
  | l == 7 && c >= 3 && c <= 12 = Pedra
  | l >= heightmap2 !! c      = Terra
  | otherwise                 = Ar

terrenoEm3 :: Int -> Int -> Terreno
terrenoEm3 l c
  | l == alturaMapa - 1 = Agua
  | l >= alturaMapa - 3 = Pedra
  | c >= 8 && c <= 12 && l >= 10 = Agua
  | l >= heightmap3 !! c = Terra
  | otherwise = Ar


mapa1 :: Mapa
mapa1 =
  [ [ terrenoEm l c | c <- [0..larguraMapa-1] ]
  | l <- [0..alturaMapa-1]
  ]

mapa2 :: Mapa
mapa2 =
  [ [ terrenoEm2 l c | c <- [0..larguraMapa-1] ]
  | l <- [0..alturaMapa-1]
  ]

mapa3 :: Mapa
mapa3 =
  [ [ terrenoEm3 l c | c <- [0 .. larguraMapa - 1] ]
  | l <- [0 .. alturaMapa - 1]
  ]

spawnEm :: [Int] -> Int -> (Int,Int)
spawnEm hm c = ((hm !! c) - 1, c)

mkMinhoca :: [Int] -> Int -> Minhoca
mkMinhoca hm col =
  Minhoca
    { posicaoMinhoca    = Just (spawnEm hm col)
    , vidaMinhoca       = Viva 100
    , jetpackMinhoca    = 0
    , escavadoraMinhoca = 0
    , bazucaMinhoca     = 0
    , minaMinhoca       = 0
    , dinamiteMinhoca   = 0
    }

estadoMapa1 :: Estado
estadoMapa1 =
  let
    b1 = Barril (spawnEm heightmap 5)  False
    b2 = Barril (spawnEm heightmap 8)  False
    b3 = Barril (spawnEm heightmap 13) False
    b4 = Barril (spawnEm heightmap 19) False
    b5 = Barril (spawnEm heightmap 23) False

    m1 = mkMinhoca heightmap 3
    m2 = mkMinhoca heightmap 22
  in
    Estado mapa1 [b1,b2,b3,b4,b5] [m1,m2]


estadoMapa2 :: Estado
estadoMapa2 =
  let
    b1 = Barril (spawnEm heightmap2 2)  False   
    b2 = Barril (spawnEm heightmap2 4)  False   
    b3 = Barril (spawnEm heightmap2 9)  False   
    b4 = Barril (spawnEm heightmap2 13) False  
    b5 = Barril (spawnEm heightmap2 22) False   

    m1 = mkMinhoca heightmap2 3
    m2 = mkMinhoca heightmap2 20
  in
    Estado mapa2 [b1,b2,b3,b4,b5] [m1,m2]


estadoMapa3 :: Estado
estadoMapa3 =
  let
    b1 = Barril (spawnEm heightmap3 2)  False
    b2 = Barril (spawnEm heightmap3 7)  False
    b3 = Barril (spawnEm heightmap3 12) False
    b4 = Barril (spawnEm heightmap3 21) False
    b5 = Barril (spawnEm heightmap3 26) False

    m1 = mkMinhoca heightmap3 3
    m2 = mkMinhoca heightmap3 18
  in
    Estado mapa3 [b1,b2,b3,b4,b5] [m1,m2]



