{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}

{-|
Module      : Desenhar
Description : Representação gráfica do jogo Angry Worms usando Gloss.
Stability   : experimental

Este módulo é responsável por converter o estado interno do jogo
('Worms' e 'Estado') em imagens ('Picture') da biblioteca Gloss.

Inclui:
* Desenho do mapa e terrenos.
* Representação gráfica das minhocas e objetos.
* Interface gráfica (HUD), barras de vida e informação do mapa.
* Menus (menu principal, ajuda e fim de jogo).
* Mira dinâmica baseada na posição do rato.

Este módulo não contém lógica de jogo, apenas desenho.
-}
module Desenhar where

import Graphics.Gloss
import Worms
import Labs2025
import Mapas

-- | Tamanho de cada célula do mapa em píxeis.
cellSize :: Float
cellSize = 40  -- 40 pixels por célula

-- | Converte uma posição da grelha (linha, coluna)
-- em coordenadas do mundo Gloss.
posToXY :: (Int, Int) -> (Float, Float)
posToXY (l,c) = (fromIntegral c * cellSize, -fromIntegral l * cellSize)

-- | Função principal de desenho do jogo.
--
-- Delega o desenho consoante o modo atual:
-- * Menu
-- * Ajuda
-- * Jogo
-- * Fim de jogo
desenha :: Worms -> Picture
desenha w =
  case modo w of
    EmMenu sel        -> desenhaMenu (recursos w) sel w
    EmAjuda           -> desenhaAjuda
    EmJogo            -> desenhaJogo w
    FimJogo resultado -> desenhaFimJogo w resultado

-- | Desenha o jogo em execução.
--
-- Inclui o mundo (mapa, minhocas, objetos, mira)
-- e a interface gráfica (HUD, barras de vida).
desenhaJogo :: Worms -> Picture
desenhaJogo (Worms est rec _ dir mp arma) =
  let mapa = mapaEstado est
      hMap = length mapa
      largura = if null mapa then 0 else length (head mapa)

      offsetX = - fromIntegral largura * cellSize / 2 + cellSize / 2
      offsetY =   fromIntegral hMap * cellSize / 2 - cellSize / 2

      mundo =
        Translate offsetX offsetY $
          Pictures
            [ desenhaMapa rec mapa
            , desenhaMira est mp offsetX offsetY
            , desenhaMinhocas rec dir (minhocasEstado est)
            , desenhaObjetos rec (objetosEstado est)
            ]

      winW = 1920
      winH = 1090

  in
    Pictures
      [ mundo
      , desenhaHUD winW winH rec arma
      , desenhaBarrasVida winW winH est
      , desenhaMapaAtual est
      ]

-- | Desenha cada tipo de terreno.
--
-- Terreno 'Ar' não é desenhado.
desenhaTerreno :: Recursos -> Terreno -> Picture
desenhaTerreno _ Ar    = Blank
desenhaTerreno rec Agua  = imgAgua rec
desenhaTerreno rec Terra = imgTerra rec
desenhaTerreno rec Pedra = imgPedra rec

-- | Desenha uma célula do mapa na posição correta.
desenhaCelula :: Recursos -> (Int, Int) -> Terreno -> Picture
desenhaCelula rec (l, c) t =
  Translate posX posY (desenhaTerreno rec t)
  where
    posX = fromIntegral c * cellSize
    posY = - fromIntegral l * cellSize

-- | Desenha o mapa completo (matriz de terrenos).
desenhaMapa :: Recursos -> Mapa -> Picture
desenhaMapa rec mapa =
  Pictures
    [ desenhaCelula rec (l, c) terreno
    | (l, linha)   <- zip [0..] mapa
    , (c, terreno) <- zip [0..] linha
    ]

-- | Desenha todas as minhocas.
desenhaMinhocas :: Recursos -> Direcao -> [Minhoca] -> Picture
desenhaMinhocas rec dir ms =
  Pictures [ desenhaMinhoca rec dir m | m <- ms ]

-- | Desenha uma minhoca individual.
--
-- A imagem é invertida horizontalmente consoante a direção.
desenhaMinhoca :: Recursos -> Direcao -> Minhoca -> Picture
desenhaMinhoca rec dir m =
  case vidaMinhoca m of
    Morta -> Blank
    Viva _ ->
      case posicaoMinhoca m of
        Nothing -> Blank
        Just p ->
          let (x,y) = posToXY p
              s  = cellSize / 785
              sx = case dir of
                     Este      -> -s
                     Nordeste  -> -s
                     Sudeste   -> -s
                     Oeste     ->  s
                     Noroeste  ->  s
                     Sudoeste  ->  s
                     _         ->  s
          in Translate x y $
               Scale sx s $
                 imgMinhoca rec

-- | Desenha todos os objetos do jogo.
desenhaObjetos :: Recursos -> [Objeto] -> Picture
desenhaObjetos rec objs = Pictures (map (desenhaObjeto rec) objs)

-- | Desenha um objeto individual (barril ou disparo).
desenhaObjeto :: Recursos -> Objeto -> Picture
desenhaObjeto rec (Barril p _) =
  let (x,y) = posToXY p
      s     = cellSize / 785
  in Translate x y $
       Scale s s $
         imgBarril rec

desenhaObjeto rec (Disparo p dir tipo _ _) =
  let (x,y) = posToXY p
      s     = cellSize / 1000
      sx    = case dir of { Este -> -s; Oeste -> s; _ -> s }
      pic   = case tipo of
                Bazuca   -> imgDisparo rec
                Mina     -> imgMina rec
                Dinamite -> imgDinamite rec
                _        -> imgDisparo rec
  in Translate x y $ Scale sx s pic

-- | Desenha o menu principal do jogo.
desenhaMenu :: Recursos -> MenuOpcao -> Worms -> Picture
desenhaMenu rec sel (Worms est _ _ _ _ _) =
  Pictures
    [ fundo
    , botoes sel
    , infoMapa
    ]
  where
    fundo = Scale 1.25 1.0547 (imgFundoMenu rec)

    infoMapa =
      let msg = "Mapa selecionado: " ++ show (numMapa est) ++ " (clicar 'M')"
      in Translate 0 (-420) $
         Pictures
           [ Color (makeColorI 0 0 0 150) (rectangleSolid 520 50)
           , Translate (-240) (-15)
               (Scale 0.25 0.25 (Color white (Text msg)))
           ]

-- | Determina o número do mapa ativo.
numMapa :: Estado -> Int
numMapa e
  | mapaEstado e == mapa1 = 1
  | mapaEstado e == mapa2 = 2
  | mapaEstado e == mapa3 = 3
  | otherwise             = 0

-- | Desenha os botões do menu.
botoes :: MenuOpcao -> Picture
botoes sel =
  Pictures
    [ botao Jogar  80
    , botao Ajuda   0
    , botao Sair  (-80)
    ]
  where
    botao op y =
      Translate 0 y $
        Pictures
          [ Color (if sel == op then makeColorI 255 200 60 255 else black)
              (rectangleSolid 200 40)
          , Color black (rectangleWire 200 40)
          , Translate (-75) (-14)
              (Scale 0.28 0.28 (Color white (Text (label op))))
          ]

-- | Texto associado a cada opção do menu.
label :: MenuOpcao -> String
label Jogar = "Jogar"
label Ajuda = "Ajuda"
label Sair  = "Sair"

-- | Desenha o ecrã de ajuda com instruções.
desenhaAjuda :: Picture
desenhaAjuda =
  Pictures
    [ Color (greyN 0.1) (rectangleSolid 3000 2000)
    , Translate (-380) 260 (Scale 0.35 0.35 (Color white (Text "AJUDA")))
    , Translate (-380) 140 (Scale 0.22 0.22 (Color white (Text "No jogo:")))
    , Translate (-380)  80 (Scale 0.2 0.2 (Color white (Text "<- / -> mover")))
    , Translate (-380)  30 (Scale 0.2 0.2 (Color white (Text "^ saltar")))
    , Translate (-380) (-20) (Scale 0.2 0.2 (Color white (Text "Space disparar")))
    , Translate (-380) (-90) (Scale 0.2 0.2 (Color white (Text "Rato: mira / apontar")))
    , Translate (-380) (-140) (Scale 0.2 0.2 (Color white (Text "1 Bazuca | 2 Mina | 3 Dinamite")))
    , Translate (-380) (-240)
        (Scale 0.2 0.2 (Color (greyN 0.8) (Text "ESC para voltar ao menu")))
    ]

-- | Desenha a linha de mira da minhoca ativa até ao rato.
desenhaMira :: Estado -> (Float, Float) -> Float -> Float -> Picture
desenhaMira est (mx,my) offsetX offsetY =
  case minhocasEstado est of
    [] -> Blank
    (m:_) ->
      case (vidaMinhoca m, posicaoMinhoca m) of
        (Viva _, Just p) ->
          let (xw,yw) = posToXY p
              alvoX = mx - offsetX
              alvoY = my - offsetY
          in Color (makeColorI 255 255 255 180) $
               Pictures
                 [ Line [(xw,yw), (alvoX,alvoY)]
                 , Translate alvoX alvoY (circleSolid 4)
                 ]
        _ -> Blank

-- | Desenha texto com o mapa atualmente ativo.
desenhaMapaAtual :: Estado -> Picture
desenhaMapaAtual est =
  let msg =
        if mapaEstado est == mapa1 then "Mapa 1"
        else if mapaEstado est == mapa2 then "Mapa 2"
        else if mapaEstado est == mapa3 then "Mapa 3"
        else "Mapa ?"
  in Translate (-940) (470) $
       Scale 0.25 0.25 $
         Color black $
           Text ("Ativo: " ++ msg)

-- | Desenha texto com contorno para melhor legibilidade.
textoMapaAtual :: Float -> Color -> String -> Picture
textoMapaAtual s cor msg =
  Pictures $
    [ Translate dx dy (Scale s s (Color black (Text msg)))
    | (dx,dy) <- [(-2,-2),(-2,0),(-2,2),(0,-2),(0,2),(2,-2),(2,0),(2,2)]
    ]
    ++
    [ Scale s s (Color cor (Text msg)) ]

-- | Desenha o HUD com as armas disponíveis.
desenhaHUD :: Float -> Float -> Recursos -> TipoArma -> Picture
desenhaHUD _ h rec armaSel =
  let
    y = h/2 - 90
    slot = 76
    gap  = 20
    padX = 26
    padY = 22
    armas = [Bazuca, Mina, Dinamite]
    nums  = ["1","2","3"]
    n = length armas
    totalSlotsW = fromIntegral n * slot + fromIntegral (n - 1) * gap
    painelW = totalSlotsW + 2 * padX
    painelH = slot + 2 * padY
    xFirst = - totalSlotsW / 2 + slot / 2

    corPainel   = makeColorI 92 62 32 210
    corBorda    = makeColorI 160 120 80 220
    corSlotSel  = makeColorI 255 220 120 60
    corSlot     = makeColorI 255 255 255 18

    painel =
      Translate 0 y $
        Pictures
          [ Color corPainel (rectangleSolid painelW painelH)
          , Color corBorda  (rectangleWire  painelW painelH)
          ]

    iconPic :: TipoArma -> Picture
    iconPic Bazuca   = imgDisparo rec
    iconPic Mina     = imgMina rec
    iconPic Dinamite = imgDinamite rec
    iconPic _        = imgDisparo rec

    slotCenterX i = xFirst + fromIntegral i * (slot + gap)

    desenhaSlot i arma num =
      let
        x = slotCenterX i
        sel = armaSel == arma
        fundoSlot =
          Translate x y $
            Color (if sel then corSlotSel else corSlot) $
              rectangleSolid slot slot
        bordaSlot =
          Translate x y $
            Pictures
              [ Color corBorda (rectangleWire slot slot)
              , if sel
                  then Color (makeColorI 255 200 60 230)
                         (rectangleWire (slot - 8) (slot - 8))
                  else Blank
              ]
        icone =
          Translate x y $
            Scale 0.06 0.06 $
              iconPic arma
        numero =
          Translate (x - slot/2 + 10) (y - slot/2 + 8) $
            Scale 0.16 0.16 $
              Color (makeColorI 255 255 255 230) $
                Text num
      in Pictures [fundoSlot, bordaSlot, icone, numero]

  in Pictures $
      [ painel ] ++
      [ desenhaSlot i arma (nums !! i) | (i, arma) <- zip [0..] armas ]

-- | Desenha uma barra de vida.
desenhaBarraVida :: Float -> Float -> Int -> Picture
desenhaBarraVida x y hp =
  let
    hp' = max 0 (min 100 (fromIntegral hp))
    wBar = 260
    hBar = 22
    pad  = 3
    frac = hp' / 100
    bg =
      Translate x y $
        Color (makeColorI 0 0 0 140) $
          rectangleSolid (wBar + 20) (hBar + 16)
    moldura =
      Translate x y $
        Color black $
          rectangleWire wBar hBar
    preenchimento =
      Translate (x - wBar/2 + (wBar*frac)/2) y $
        Color (makeColorI 80 220 120 255) $
          rectangleSolid (wBar * frac - pad) (hBar - pad)
  in Pictures [bg, moldura, preenchimento]

-- | Extrai os pontos de vida de uma minhoca.
hpMinhoca :: Minhoca -> Int
hpMinhoca m =
  case vidaMinhoca m of
    Morta   -> 0
    Viva hp -> hp

-- | Desenha as barras de vida das minhocas.
desenhaBarrasVida :: Float -> Float -> Estado -> Picture
desenhaBarrasVida w h est =
  let
    ms = minhocasEstado est
    yBase = -h/2 + 60
    xEsq  = -w/2 + 170
    xDir  =  w/2 - 170
    hp0 = case ms of
            (m0:_) -> hpMinhoca m0
            _      -> 0
    hp1 = case ms of
            (_:m1:_) -> hpMinhoca m1
            _        -> 0
  in Pictures
      [ desenhaBarraVida xEsq yBase hp0
      , desenhaBarraVida xDir yBase hp1
      ]

-- | Desenha o ecrã de fim de jogo com o vencedor.
desenhaFimJogo :: Worms -> Resultado -> Picture
desenhaFimJogo w (Vencedor i) =
  let
    winW = 1920
    winH = 1090
    rec = recursos w
    img =
      case i of
        0 -> imgVencedor1 rec
        1 -> imgVencedor2 rec
        _ -> Blank
  in
    Pictures
      [ Color (makeColorI 0 0 0 220)
          (rectangleSolid winW winH)
      , Scale 0.8 0.8 img
      ]
