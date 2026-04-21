{-|
Module      : Tempo
Description : Evolução temporal do jogo (gravidade, movimento e explosões).
Stability   : experimental

Este módulo implementa a evolução do estado do jogo ao longo do tempo,
sendo chamado a cada tick pelo Gloss através da função 'reageTempo'.

Responsabilidades:
* Aplicar gravidade a minhocas, barris, minas e dinamites.
* Avançar disparos e detetar colisões com o mapa e barris.
* Gerir explosões (barris, minas pisadas e temporizadores).
* Aplicar dano às minhocas.
* Verificar condição de vitória.
-}
module Tempo where

import Worms
import Labs2025
import Labs2025 (Posicao)

-- | Tempo em segundos.
type Segundos = Float



-- | Função que avança o tempo no estado do jogo no Gloss.
--
-- Esta função é executada apenas quando o jogo está em modo 'EmJogo'.
-- A sequência de passos é:
--
-- 1. Aplicar gravidade às minhocas e objetos.
-- 2. Resolver colisões tiro–minhoca antes do movimento.
-- 3. Atualizar temporizadores de minas e dinamites.
-- 4. Mover disparos e resolver colisões com mapa e barris.
-- 5. Detetar minas pisadas.
-- 6. Aplicar explosões resultantes.
-- 7. Resolver colisões tiro–minhoca após explosões.
-- 8. Aplicar gravidade aos barris.
-- 9. Remover disparos fora do mapa.
-- 10. Verificar condição de vitória.
reageTempo :: Segundos -> Worms -> Worms
reageTempo dt w@(Worms est rec modoAtual dir mp arma) =
  case modoAtual of
    EmJogo ->
      let
        -- 1) gravidade
        (Estado mapa0 objs0 mins0) = aplicaGravidade est

        -- 2) tiros vs minhocas ANTES de mover
        (mins0', objs0') = resolveTirosMinhocas mins0 objs0

        -- 2.5) timers (mina/dinamite)
        (explT, objsT) = tickTiros objs0'

        -- 3) mover tiros + colisões
        (mapa1, objs1, expl) = avancaObjetos mapa0 objsT

        -- 3.5) minas pisadas
        (explP, objsP) = explodeMinasPisadas mins0' objs1

        -- 4) explosões
        Estado mapaE objsE minsE =
          aplicaExplosoes (expl ++ explT ++ explP)
                          (Estado mapa1 objsP mins0')

        -- 5) tiros vs minhocas DEPOIS das explosões
        (mins1, objs1') = resolveTirosMinhocas minsE objsE

        -- 6) gravidade dos barris
        objs2 = [ o | Just o <- map (cairBarril mapaE) objs1' ]

        -- 7) remover tiros fora do mapa
        objsFinal = removeDisparosForaMapa mapaE objs2

        -- verificar vencedor
        vivas =
          [ i
          | (i,m) <- zip [0..] mins1
          , vidaMinhoca m /= Morta
          ]
      in
        case vivas of
          [i] ->
            Worms (Estado mapaE objsFinal mins1)
                  rec
                  (FimJogo (Vencedor i))
                  dir mp arma
          _  ->
            Worms (Estado mapaE objsFinal mins1)
                  rec
                  modoAtual
                  dir mp arma

    _ ->
      w



-- | Aplica gravidade a todas as entidades do estado.
--
-- Minhocas caem verticalmente e morrem ao entrar em 'Agua'.
-- Minas e dinamites também são afetadas pela gravidade.
aplicaGravidade :: Estado -> Estado
aplicaGravidade est@(Estado mapa objs minhocas) =
    est 
    { minhocasEstado = map (cairMinhoca mapa) minhocas
    , objetosEstado  = map (cairMinaDinamite mapa) objs }

-- | Aplica gravidade a uma única minhoca.
cairMinhoca :: Mapa -> Minhoca -> Minhoca
cairMinhoca mapa m =
  case posicaoMinhoca m of
    Nothing -> m
    Just (l,c) ->
      case lookupMapa mapa (l,c) of
        Just Agua -> m { vidaMinhoca = Morta }
        _ ->
          let abaixo = (l+1,c)
          in case lookupMapa mapa abaixo of
               Just Ar   -> m { posicaoMinhoca = Just abaixo }
               Just Agua -> m { posicaoMinhoca = Just abaixo
                              , vidaMinhoca   = Morta }
               _         -> m


-- | Verifica se dois objetos ocupam a mesma posição.
mesmaPosicao :: Objeto -> Objeto -> Bool
mesmaPosicao (Disparo { posicaoDisparo = p1 })
             (Barril  { posicaoBarril  = p2 }) = p1 == p2
mesmaPosicao (Barril  { posicaoBarril  = p1 })
             (Disparo { posicaoDisparo = p2 }) = p1 == p2
mesmaPosicao _ _ = False


-- | Remove disparos que saem fora dos limites do mapa.
removeDisparosForaMapa :: Mapa -> [Objeto] -> [Objeto]
removeDisparosForaMapa mapa =
  filter dentro
  where
    dentro (Disparo { posicaoDisparo = p }) = dentroMapa mapa p
    dentro _ = True


-- | Atualiza uma célula do mapa (l,c).
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


-- | Move disparos e resolve colisões com o mapa e com barris.
--
-- Devolve:
-- * Novo mapa
-- * Lista atualizada de objetos
-- * Posições onde ocorreram explosões
avancaObjetos :: Mapa -> [Objeto] -> (Mapa, [Objeto], [Posicao])
avancaObjetos mapa objs =
  let
    posBarris :: [Posicao]
    posBarris = [ p | Barril { posicaoBarril = p } <- objs ]

    step :: (Mapa, [Objeto], [Posicao]) -> Objeto -> (Mapa, [Objeto], [Posicao])
    step (m, acc, expl) o@Barril{} =
      (m, o : acc, expl)

    step (m, acc, expl)
         d@Disparo{ posicaoDisparo = p@(l,c)
                  , direcaoDisparo = dir
                  , tipoDisparo    = tipo
                  } =
      if p `elem` posBarris
        then
          (m, acc, p : expl)
        else
          let
            p' =
              case tipo of
                Mina ->
                  p

                Dinamite ->
                  (l + 1, c)

                _ ->
                  let
                    (dl, dc) =
                      case dir of
                        Este      -> ( 0,  1)
                        Oeste     -> ( 0, -1)
                        Norte     -> (-1,  0)
                        Sul       -> ( 1,  0)
                        Nordeste  -> (-1,  1)
                        Noroeste  -> (-1, -1)
                        Sudeste   -> ( 1,  1)
                        Sudoeste  -> ( 1, -1)
                        _         -> ( 0,  0)
                  in
                    (l + dl, c + dc)
          in
            case lookupMapa m p' of
              Nothing ->
                (m, acc, expl)

              Just Ar ->
                if p' `elem` posBarris
                  then
                    (m, acc, p' : expl)
                  else
                    (m, d { posicaoDisparo = p' } : acc, expl)

              Just Terra ->
                case tipo of
                  Dinamite ->
                    (m, d : acc, expl)
                  Mina ->
                    (m, d : acc, expl)
                  _ ->
                    (setMapa m p' Ar, acc, expl)

              Just Pedra ->
                (m, acc, expl)

              Just Agua ->
                (m, acc, expl)

    (mapaDepois, objsDepois, expl) =
      foldl step (mapa, [], []) objs

    objsFinal =
      [ o
      | o <- reverse objsDepois
      , case o of
          Barril { posicaoBarril = p } -> not (p `elem` expl)
          _                            -> True
      ]
  in
    (mapaDepois, objsFinal, expl)


-- | Deteta minas pisadas por minhocas vivas.
explodeMinasPisadas :: [Minhoca] -> [Objeto] -> ([Posicao], [Objeto])
explodeMinasPisadas ms objs =
  let vivasPos = [ p | m <- ms, vidaMinhoca m /= Morta, Just p <- [posicaoMinhoca m] ]
  in foldr (\o (expl, acc) ->
        case o of
          Disparo{posicaoDisparo=p, tipoDisparo=Mina} | p `elem` vivasPos ->
            (p:expl, acc)
          _ -> (expl, o:acc)
     ) ([], []) objs


-- | Gravidade para minas e dinamites (modeladas como disparos).
cairMinaDinamite :: Mapa -> Objeto -> Objeto
cairMinaDinamite mapa d@Disparo{posicaoDisparo=(l,c), tipoDisparo=tipo}
  | tipo == Mina || tipo == Dinamite =
      case lookupMapa mapa (l+1,c) of
        Just Ar   -> d{posicaoDisparo=(l+1,c)}
        Just Agua -> d{posicaoDisparo=(l+1,c)}  
        _         -> d
cairMinaDinamite _ o = o


-- | Aplica gravidade a um barril.
--
-- Barris desaparecem ao cair em 'Agua'.
cairBarril :: Mapa -> Objeto -> Maybe Objeto
cairBarril mapa b@Barril{posicaoBarril=(l,c)} =
  case lookupMapa mapa (l+1,c) of
    Just Ar   -> Just b{posicaoBarril=(l+1,c)}
    Just Agua -> Nothing
    _         -> Just b
cairBarril _ o = Just o


-- | Resolve colisões disparo <-> minhoca.
--
-- Se um disparo atingir uma minhoca viva, aplica dano
-- e remove o disparo.
resolveTirosMinhocas :: [Minhoca] -> [Objeto] -> ([Minhoca], [Objeto])
resolveTirosMinhocas ms objs =
  let
    posTiros :: [Posicao]
    posTiros = [ p | Disparo{posicaoDisparo=p} <- objs ]

    posVivas :: [Posicao]
    posVivas =
      [ p
      | m <- ms
      , vidaMinhoca m /= Morta
      , Just p <- [posicaoMinhoca m]
      ]

    ms' = map (levaDanoSeAtingida posTiros) ms

    objs' =
      [ o
      | o <- objs
      , case o of
          Disparo{posicaoDisparo=p} -> not (p `elem` posVivas)
          _                         -> True
      ]
  in
    (ms', objs')


-- | Aplica dano a uma minhoca se esta for atingida.
levaDanoSeAtingida :: [Posicao] -> Minhoca -> Minhoca
levaDanoSeAtingida tiros m =
  case vidaMinhoca m of
    Morta   -> m
    Viva hp ->
      case posicaoMinhoca m of
        Nothing -> m
        Just p  ->
          if p `elem` tiros
            then
              let dano = 25
                  hp'  = hp - dano
              in if hp' <= 0
                   then m { vidaMinhoca = Morta }
                   else m { vidaMinhoca = Viva hp' }
            else m


-- | Vizinhança 3x3 (raio 1) à volta de uma posição.
vizinhosR1 :: Posicao -> [Posicao]
vizinhosR1 (l,c) =
  [ (l+dl, c+dc)
  | dl <- [-1..1]
  , dc <- [-1..1]
  ]


-- | Verifica se uma posição está dentro do mapa.
dentroMapa :: Mapa -> Posicao -> Bool
dentroMapa mapa (l,c) =
  l >= 0 && l < length mapa &&
  c >= 0 && c < length (head mapa)


-- | Aplica explosões (barris, minas e dinamites).
--
-- Efeitos:
-- * Destrói 'Terra' num raio 1.
-- * Aplica dano às minhocas na área.
-- * Remove disparos dentro da área.
aplicaExplosoes :: [Posicao] -> Estado -> Estado
aplicaExplosoes expl est@(Estado mapa objs mins) =
  let
    area = filter (dentroMapa mapa) (concatMap vizinhosR1 expl)

    mapa' = foldl destroiTerra mapa area
    destroiTerra m p =
      case lookupMapa m p of
        Just Terra -> setMapa m p Ar
        _          -> m

    mins' = map (danoMinhoca area) mins
    danoMinhoca areaDano m =
      case (vidaMinhoca m, posicaoMinhoca m) of
        (Morta, _) -> m
        (Viva v, Just p)
          | p `elem` areaDano ->
              let v' = v - 75
              in if v' <= 0 then m { vidaMinhoca = Morta }
                 else m { vidaMinhoca = Viva v' }
        _ -> m

    objs' =
      [ o
      | o <- objs
      , case o of
          Disparo{posicaoDisparo=p} -> not (p `elem` area)
          _                         -> True
      ]

  in Estado mapa' objs' mins'


-- | Atualiza temporizadores de minas e dinamites.
--
-- Quando o temporizador chega a zero, gera uma explosão.
tickTiros :: [Objeto] -> ([Posicao], [Objeto])
tickTiros = foldr step ([], [])
  where
    step o (expl, acc) =
      case o of
        Disparo { posicaoDisparo = p, tempoDisparo = Just t } ->
          let t' = t - 1
          in if t' <= 0
                then (p : expl, acc)
                else (expl, o { tempoDisparo = Just t' } : acc)
        _ ->
          (expl, o : acc)
