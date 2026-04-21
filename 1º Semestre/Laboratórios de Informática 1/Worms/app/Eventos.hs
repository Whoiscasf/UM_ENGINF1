{-|
Module      : Eventos
Description : Tratamento de eventos do utilizador (teclado e rato) no Gloss.
Stability   : experimental

Este módulo interpreta eventos do Gloss ('Event') e atualiza o estado global
do jogo ('Worms').

Responsabilidades:
* Encaminhar eventos conforme o modo atual (Menu/Ajuda/Jogo/Fim).
* Controlos do jogo: mover, saltar, disparar e selecionar arma.
* Guardar posição do rato para mira e calcular direção a partir do rato.
* Navegação no menu e retorno ao menu via ESC.
-}
module Eventos where

import Graphics.Gloss.Interface.Pure.Game

import Worms

import Labs2025

import Mapas 

-- | Tamanho de cada célula do mapa em píxeis.
cellSize :: Float
cellSize = 40

-- | Converte uma posição da grelha (linha, coluna) para coordenadas do mundo Gloss.
posToXY :: (Int, Int) -> (Float, Float)
posToXY (l,c) = (fromIntegral c * cellSize, -fromIntegral l * cellSize)

-- | Função principal que altera o estado do jogo no Gloss em resposta a um 'Event'.
--
-- Encaminha o evento para o manipulador do modo atual:
-- * 'EmMenu'  -> 'menuEvento'
-- * 'EmAjuda' -> 'ajudaEvento'
-- * 'EmJogo'  -> 'jogoEvento'
-- * 'FimJogo' -> 'fimJogoEvento'
reageEventos :: Event -> Worms -> Worms
reageEventos ev w =
  case modo w of
    EmMenu sel        -> menuEvento ev sel w
    EmAjuda           -> ajudaEvento ev w
    EmJogo            -> jogoEvento ev w
    FimJogo _         -> fimJogoEvento ev w

-- | Eventos durante o jogo.
--
-- Controles:
-- * Left/Right: mover
-- * Up: saltar
-- * Space / clique esquerdo: disparar
-- * 1/2/3: escolher arma
-- * Movimento do rato: atualizar mira
-- * ESC: voltar ao menu
jogoEvento :: Event -> Worms -> Worms 

jogoEvento (EventKey (SpecialKey KeyLeft) Down _ _) (Worms est rec modoAtual _ mp arma) =
  Worms (moverJogador Oeste est) rec modoAtual Oeste mp arma 

jogoEvento (EventKey (SpecialKey KeyRight) Down _ _) (Worms est rec modoAtual _ mp arma) =
  Worms (moverJogador Este est) rec modoAtual Este mp arma

jogoEvento (EventKey (SpecialKey KeyUp) Down _ _) (Worms est rec modoAtual dir mp arma) =
  Worms (saltaMinhoca est) rec modoAtual dir mp arma 

jogoEvento (EventKey (SpecialKey KeySpace) Down _ _) (Worms est rec modoAtual dir mp arma) =
  let est' = criaDisparo arma dir est
  in Worms (passaTurno est') rec modoAtual dir mp arma

jogoEvento (EventKey (MouseButton LeftButton) Down _ _) (Worms est rec modoAtual _dir mp arma) =
  let dir' = direcaoPeloRato8 mp est
      est' = criaDisparo arma dir' est
  in Worms (passaTurno est') rec modoAtual dir' mp arma 

-- | Escolher arma com números.
jogoEvento (EventKey (Char '1') Down _ _) (Worms est rec modoAtual dir mp _arma) =
  Worms est rec modoAtual dir mp Bazuca

jogoEvento (EventKey (Char '2') Down _ _) (Worms est rec modoAtual dir mp _arma) =
  Worms est rec modoAtual dir mp Mina

jogoEvento (EventKey (Char '3') Down _ _) (Worms est rec modoAtual dir mp _arma) =
  Worms est rec modoAtual dir mp Dinamite

-- | Guarda posição do rato (para linha de mira).
jogoEvento (EventMotion mp) (Worms est rec modoAtual dir _ arma) =
  Worms est rec modoAtual dir mp arma

-- | ESC volta ao menu.
jogoEvento (EventKey (SpecialKey KeyEsc) Down _ _) (Worms est rec _ dir mp arma) =
  Worms est rec (EmMenu Jogar) dir mp arma

jogoEvento _ w = w

-- | Eventos do menu principal:
-- * Up/Down para navegar
-- * Enter/Space para confirmar
-- * 'm' para alternar o mapa
menuEvento :: Event -> MenuOpcao -> Worms -> Worms
menuEvento ev sel (Worms est rec _ dir mp arma ) =
  case ev of
    EventKey (SpecialKey KeyUp) Down _ _ ->
      Worms est rec (EmMenu (anterior sel)) dir mp arma

    EventKey (SpecialKey KeyDown) Down _ _ ->
      Worms est rec (EmMenu (proxima sel)) dir mp arma

    EventKey (SpecialKey KeyEnter) Down _ _ ->
      executa sel (Worms est rec (EmMenu sel) dir mp arma)

    EventKey (SpecialKey KeySpace) Down _ _ ->
      executa sel (Worms est rec (EmMenu sel) dir mp arma)

    -- tecla M: trocar mapa
    EventKey (Char 'm') Down _ _ ->
      let novoEst 
            | mapaEstado est == mapa1 = estadoMapa2
            | mapaEstado est == mapa2 = estadoMapa3
            | otherwise              = estadoMapa1
      in Worms novoEst rec (EmMenu sel) dir mp arma

    _ ->
      Worms est rec (EmMenu sel) dir mp arma

-- | Executa a opção selecionada no menu.
executa :: MenuOpcao -> Worms -> Worms
executa Jogar (Worms est rec _ dir mp arma) =
  Worms est rec EmJogo dir mp arma

executa Ajuda (Worms est rec _ dir mp arma) =
  Worms est rec EmAjuda dir mp arma

executa Sair (Worms est rec _ dir mp arma) =
  Worms est rec (EmMenu Jogar) dir mp arma

-- | Eventos do ecrã de ajuda: ESC volta ao menu.
ajudaEvento :: Event -> Worms -> Worms
ajudaEvento ev (Worms est rec _ dir mp arma) =
  case ev of
    EventKey (SpecialKey KeyEsc) Down _ _ -> Worms est rec (EmMenu Jogar) dir mp arma
    _ -> Worms est rec EmAjuda dir mp arma

-- | Move a minhoca ativa na direção indicada (apenas horizontal).
moverJogador :: Direcao -> Estado -> Estado
moverJogador dir (Estado mapa objs (m:ms)) =
  case posicaoMinhoca m of
    Nothing -> Estado mapa objs (m:ms)
    Just (l,c) ->
      let dc = case dir of
                 Este  ->  1
                 Oeste -> -1
                 _     ->  0
          nova = (l, c + dc)
      in if dc == 0
         then Estado mapa objs (m:ms)
         else case lookupMapa mapa nova of
                Just Ar   -> Estado mapa objs (m { posicaoMinhoca = Just nova } : ms)
                Just Agua -> Estado mapa objs (m { posicaoMinhoca = Just nova } : ms)
                _         -> Estado mapa objs (m:ms)
moverJogador _ est = est

-- | Faz a minhoca ativa saltar uma célula para cima, se houver espaço.
saltaMinhoca :: Estado -> Estado
saltaMinhoca est@(Estado _ _ []) = est
saltaMinhoca (Estado mapa objs (m:ms)) =
  case vidaMinhoca m of
    Morta -> Estado mapa objs (m:ms)
    Viva _ ->
      case posicaoMinhoca m of
        Nothing -> Estado mapa objs (m:ms)
        Just (l, c) ->
          let cima = (l - 1, c)
          in case lookupMapa mapa cima of
               Just Ar -> Estado mapa objs (m { posicaoMinhoca = Just cima } : ms)
               _       -> Estado mapa objs (m:ms)

-- | Duração (em ticks) associada a armas temporizadas.
tempoArma :: TipoArma -> Maybe Ticks
tempoArma Bazuca   = Nothing
tempoArma Mina     = Just 180     
tempoArma Dinamite = Just 240     
tempoArma _        = Nothing

-- | Cria um disparo à frente da minhoca ativa ou destrói 'Terra' se for atingida.
criaDisparo ::TipoArma -> Direcao -> Estado -> Estado
criaDisparo _ _ est@(Estado _ _ []) = est
criaDisparo arma dir est@(Estado mapa objs (m:ms)) =
  case vidaMinhoca m of
    Morta  -> est
    Viva _ ->
      case posicaoMinhoca m of
        Nothing -> est
        Just (l,c) ->
          let
            (dl, dc, dirTiro) =
              case dir of
                Este      -> ( 0,  1, Este)
                Oeste     -> ( 0, -1, Oeste)
                Norte     -> (-1,  0, Norte)
                Sul       -> ( 1,  0, Sul)
                Nordeste  -> (-1,  1, Nordeste)
                Noroeste  -> (-1, -1, Noroeste)
                Sudeste   -> ( 1,  1, Sudeste)
                Sudoeste  -> ( 1, -1, Sudoeste)
                _         -> ( 0,  1, Este)

            front = (l + dl, c + dc)
          in
            case lookupMapa mapa front of
              Just Ar ->
                let novoDisparo = Disparo
                      { posicaoDisparo = front
                      , direcaoDisparo = dirTiro
                      , tipoDisparo    = arma
                      , tempoDisparo   = tempoArma arma
                      , donoDisparo    = 0
                      }
                in Estado mapa (novoDisparo:objs) (m:ms)

              Just Terra ->
                let mapa' = setMapa mapa front Ar
                in Estado mapa' objs (m:ms)

              _ -> est

-- | Atualiza uma célula do mapa (l,c) para um novo 'Terreno'.
setMapa :: Mapa -> (Int,Int) -> Terreno -> Mapa
setMapa mapa (l,c) t
  | l < 0 || l >= length mapa = mapa
  | c < 0 || c >= length (head mapa) = mapa
  | otherwise =
      take l mapa
      ++ [ setLinha (mapa !! l) c t ]
      ++ drop (l+1) mapa
  where
    setLinha linha i novo =
      take i linha ++ [novo] ++ drop (i+1) linha

-- | Passa o turno para a próxima minhoca (rota a lista).
passaTurno :: Estado -> Estado
passaTurno est@(Estado mapa objs ms) =
  case ms of
    []      -> est
    (m:rest) -> est { minhocasEstado = rest ++ [m] }

-- | Passa turnos ignorando minhocas mortas (se necessário).
pulaMortas :: Estado -> Estado
pulaMortas est@(Estado _ _ ms) =
  case ms of
    [] -> est
    (m:rest) ->
      case vidaMinhoca m of
        Morta -> pulaMortas (passaTurno est)
        _     -> est

-- | Calcula a direção discreta (8 direções) a partir da posição do rato.
direcaoPeloRato8 :: (Float,Float) -> Estado -> Direcao
direcaoPeloRato8 mp est@(Estado _ _ (m:_)) =
  case posicaoMinhoca m of
    Nothing -> Este
    Just p  ->
      let (mx,my) = mouseParaMundo est mp
          (xw,yw) = posToXY p
          dx = mx - xw
          dy = my - yw
          a  = atan2 dy dx
      in angToDir a
direcaoPeloRato8 _ _ = Este

-- | Converte um ângulo (radianos) numa direção discreta (8 direções).
angToDir :: Float -> Direcao
angToDir a
  | a >= (-pi/8)   && a < (pi/8)    = Este
  | a >= (pi/8)    && a < (3*pi/8)  = Nordeste
  | a >= (3*pi/8)  && a < (5*pi/8)  = Norte
  | a >= (5*pi/8)  && a < (7*pi/8)  = Noroeste
  | a >= (7*pi/8)  || a < (-7*pi/8) = Oeste
  | a >= (-7*pi/8) && a < (-5*pi/8) = Sudoeste
  | a >= (-5*pi/8) && a < (-3*pi/8) = Sul
  | otherwise                       = Sudeste

-- | Calcula offsets usados para centrar o mapa no ecrã (coerente com o desenho).
offsetsEstado :: Estado -> (Float, Float)
offsetsEstado est =
  let mapa = mapaEstado est
      h = length mapa
      w = if null mapa then 0 else length (head mapa)
      offsetX = - fromIntegral w * cellSize / 2 + cellSize / 2
      offsetY =   fromIntegral h * cellSize / 2 - cellSize / 2
  in (offsetX, offsetY)

-- | Converte coordenadas do rato (janela) para coordenadas do mundo (mapa).
mouseParaMundo :: Estado -> (Float, Float) -> (Float, Float)
mouseParaMundo est (mx,my) =
  let (ox,oy) = offsetsEstado est
  in (mx - ox, my - oy)

-- | Eventos no fim de jogo.
--
-- ESC ou Enter regressam ao menu principal; outros eventos são ignorados.
fimJogoEvento :: Event -> Worms -> Worms
fimJogoEvento (EventKey (SpecialKey KeyEsc) Down _ _) w =
  -- volta ao menu (ajusta se o teu menu inicial for diferente)
  w { modo = EmMenu Jogar }

fimJogoEvento (EventKey (SpecialKey KeyEnter) Down _ _) w =
  w { modo = EmMenu Jogar }

-- ignora TODOS os outros eventos (mouse move, etc.)
fimJogoEvento _ w = w
