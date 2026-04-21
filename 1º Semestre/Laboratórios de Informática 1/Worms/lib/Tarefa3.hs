{-|
Module      : Tarefa3
Description : Avançar tempo do jogo (Tarefa 3)
-}
module Tarefa3 where

import Data.Either 
import Data.Maybe 

import Labs2025
import Tarefa0_2025 
import Tarefa2

type Dano = Int
type Danos = [(Posicao,Dano)]

-- | Avança o estado do jogo um 'tick'.
avancaEstado :: Estado -> Estado
avancaEstado (Estado mapa objetos minhocas) = foldl (flip aplicaDanos) e_com_objetos danos_das_explosoes
    where
    e = Estado mapa objetos minhocas
    -- Avança minhocas (gravidade)
    minhocas' = [avancaMinhoca e i m | (i, m) <- zip [0..] minhocas]
    e_com_minhocas = e { minhocasEstado = minhocas' }

    -- Avança objetos, separando os que ficam dos que explodem
    resultados_obj = [avancaObjeto e_com_minhocas i o | (i, o) <- zip [0..] (objetosEstado e_com_minhocas)]
    
    objetos' = lefts resultados_obj
    danos_das_explosoes = rights resultados_obj -- Esta é uma lista de Danos: [[(Pos, Dano)]]
    
    e_com_objetos = e_com_minhocas { objetosEstado = objetos' }
    -- Por fim, o foldl aplica cada explosão (cada 'Danos') ao estado

-- | Avança o estado de uma minhoca individual (aplica gravidade se necessário).
avancaMinhoca :: Estado -> NumMinhoca -> Minhoca -> Minhoca
avancaMinhoca e _ m
    | not (minhocaViva m) = m
    | otherwise =
        case posicaoMinhoca m of
            Nothing -> m
            Just p  ->
                if minhocaDeveCair m e
                then avancaGravidade p m e
                else m

-- | Verifica se uma minhoca deve sofrer o efeito da gravidade.
minhocaDeveCair :: Minhoca -> Estado -> Bool
minhocaDeveCair m e =
    case situacaoMinhoca m e of
        NoChao -> False
        _      -> True

-- | Função intermédia para aplicar gravidade.
avancaGravidade :: Posicao -> Minhoca -> Estado -> Minhoca
avancaGravidade p m e = resolveQueda p e m

-- | Resolve a lógica de queda de uma minhoca.
resolveQueda :: Posicao -> Estado -> Minhoca -> Minhoca
resolveQueda pAnterior e m =
    let posAbaixo = somaPos pAnterior (offsetDirecao Sul)
        mp = mapaEstado e
    in
    if not (posicaoInBounds posAbaixo mp) then
        matarMinhoca m Nothing
    else
        if ePosicaoEstadoLivre posAbaixo e then
            case terrenoEm posAbaixo mp of
                Agua -> matarMinhoca m (Just posAbaixo)
                Ar   -> m { posicaoMinhoca = Just posAbaixo }
                _    -> m { posicaoMinhoca = Just pAnterior }
        else
            m { posicaoMinhoca = Just pAnterior }

-- | Avança o estado de um objeto.
-- | Retorna 'Left Objeto' se o objeto continuar, 'Right Danos' se explodir.
avancaObjeto :: Estado -> NumObjeto -> Objeto -> Either Objeto Danos

-- | Lógica da Bazuca (estilo if-then-else)
avancaObjeto e _ (Disparo pos dir Bazuca t dono) =
    let posProxima = movePosicao dir pos
        mp = mapaEstado e
        diam = obtemDiametro (Disparo pos dir Bazuca t dono)
    in
    if not (posicaoInBounds posProxima mp) then
        Right [] -- Bate na borda
    else if terrenoEm posProxima mp == Agua then
        Right (calculaDanos posProxima diam) -- Bate na água
    else if not (ePosicaoEstadoLivre posProxima e) then
        Right (calculaDanos posProxima diam) -- Bate em algo (minhoca, terra)
    else
        Left (Disparo posProxima dir Bazuca t dono) -- Continua a voar

-- | Lógica para outros objetos (Barril, Mina, Dinamite).
avancaObjeto e _ o =
    let posicao = obtemPosicaoObjeto o
    in
    if prontoExplodir o e && isJust posicao
    then
        Right (calculaDanos (fromJust posicao) (obtemDiametro o))
    else
        Left (avancaFisica o e)

-- | Verifica se um Barril está ativo e pronto a explodir.
eBarrilAExplodir :: Objeto -> Bool
eBarrilAExplodir (Barril _ True) = True
eBarrilAExplodir _               = False

-- | Verifica se um disparo com 'timer' (Mina, Dinamite) chegou a zero.
eDisparoTemporizadoAExplodir :: Objeto -> Bool
eDisparoTemporizadoAExplodir (Disparo _ _ _ (Just 0) _) = True
eDisparoTemporizadoAExplodir _                          = False

-- | Verifica se uma Bazuca colidiu.
eBazucaAExplodir :: Objeto -> Estado -> Bool
eBazucaAExplodir (Disparo pos _ Bazuca _ _) e = not (ePosicaoEstadoLivre pos e)
eBazucaAExplodir _ _                          = False

-- | Verifica se um objeto (exceto Bazuca) está pronto a explodir.
prontoExplodir :: Objeto -> Estado -> Bool
prontoExplodir o _ =
    eBarrilAExplodir o ||
    eDisparoTemporizadoAExplodir o

-- | Aplica uma lista de danos (de uma explosão) ao estado do jogo.
aplicaDanos :: Danos -> Estado -> Estado
aplicaDanos ds e =
    let
        posicoesDano = map fst ds
        mapaNovo = destroiPosicoes posicoesDano (mapaEstado e)
        minhocasNovas = map (aplicaDanoAMinhoca ds) (minhocasEstado e)
        objetosNovos = map (aplicaDanoAObjeto posicoesDano) (objetosEstado e)
    in
        e { mapaEstado = mapaNovo, objetosEstado = objetosNovos, minhocasEstado = minhocasNovas }

-- | Aplica o dano a um objeto, potencialmente ativando-o (reação em cadeia).
aplicaDanoAObjeto :: [Posicao] -> Objeto -> Objeto
aplicaDanoAObjeto posicoesDano (Barril pos False) =
    if pos `elem` posicoesDano
    then Barril pos True
    else Barril pos False

aplicaDanoAObjeto posicoesDano (Disparo pos dir Mina (Just t) dono) =
    if t > 0 && pos `elem` posicoesDano
    then Disparo pos dir Mina (Just 0) dono
    else Disparo pos dir Mina (Just t) dono

aplicaDanoAObjeto posicoesDano (Disparo pos dir Dinamite (Just t) dono) =
    if t > 0 && pos `elem` posicoesDano
    then Disparo pos dir Dinamite (Just 0) dono
    else Disparo pos dir Dinamite (Just t) dono

aplicaDanoAObjeto _ obj = obj

-- | Calcula o dano total acumulado numa única posição.
danoTotalNaPosicao :: Posicao -> Danos -> Int
danoTotalNaPosicao pos ds = sum [d | (p, d) <- ds, p == pos]

-- | Aplica o dano de uma explosão a uma minhoca.
aplicaDanoAMinhoca :: Danos -> Minhoca -> Minhoca
aplicaDanoAMinhoca ds m =
    case posicaoMinhoca m of
        Nothing -> m
        Just pos ->
            let danoRecebido = danoTotalNaPosicao pos ds
            in if danoRecebido == 0
               then m
               else resolveDanoMinhoca m danoRecebido

-- | Resolve a aplicação de dano à vida de uma minhoca.
resolveDanoMinhoca :: Minhoca -> Dano -> Minhoca
resolveDanoMinhoca m danoRecebido =
    case vidaMinhoca m of
        Morta -> m
        Viva vida ->
            let novaVida = vida - danoRecebido
            in if novaVida <= 0
               then matarMinhoca m Nothing
               else m { vidaMinhoca = Viva novaVida }

-- | Destrói uma lista de posições no mapa.
destroiPosicoes :: [Posicao] -> Mapa -> Mapa
destroiPosicoes posicoes mapa = foldr destroiPosicao mapa posicoes

-- | Avança a física de um objeto (gravidade, 'timers').
avancaFisica :: Objeto -> Estado -> Objeto
avancaFisica (Barril pos False) e
    | posChao pos (mapaEstado e) = Barril pos False
    | otherwise =
        let posAbaixo = movePosicao Sul pos
            mp = mapaEstado e
        in
        if not (posicaoInBounds posAbaixo mp) then
            Barril posAbaixo True
        else
            if ePosicaoEstadoLivre posAbaixo e then
                case terrenoEm posAbaixo mp of
                    Agua -> Barril { posicaoBarril = posAbaixo, explodeBarril = True }
                    Ar   -> Barril { posicaoBarril = posAbaixo, explodeBarril = False }
                    _    -> Barril { posicaoBarril = pos, explodeBarril = False }
            else
                Barril pos False

avancaFisica (Barril pos True) _ = Barril pos True
avancaFisica (Disparo pos dir Bazuca t dono) _ = Disparo (movePosicao dir pos) dir Bazuca t dono
avancaFisica (Disparo pos dir Mina mt dono) e = avancaFisicaMina (Disparo pos dir Mina mt dono) e
avancaFisica (Disparo pos dir Dinamite (Just t) dono) e = avancaFisicaDinamite (Disparo pos dir Dinamite (Just t) dono) e
avancaFisica d _ = d

-- | Avança a física específica da Mina (gravidade, 'timer', ativação por proximidade).
avancaFisicaMina :: Objeto -> Estado -> Objeto
avancaFisicaMina (Disparo pos dir Mina mt dono) e =
  let
    mapa = mapaEstado e
    noChao = posChao pos mapa
    (posNova, novaDir) = calculaMovimentoMina pos dir noChao
    minhocas = minhocasEstado e
    mtNovo = calculaTempoMina mt pos dono minhocas
  in
    Disparo { posicaoDisparo = posNova, direcaoDisparo = novaDir, tipoDisparo = Mina, tempoDisparo = mtNovo, donoDisparo = dono }
avancaFisicaMina o _ = o

-- | Calcula o movimento da Mina. Cai sempre para Sul e aponta para Norte.
calculaMovimentoMina :: Posicao -> Direcao -> Bool -> (Posicao, Direcao)
calculaMovimentoMina pos _ noChao
    | noChao    = (pos, Norte)
    | otherwise = (movePosicao Sul pos, Norte)

-- | Calcula o 'timer' da Mina.
calculaTempoMina :: Maybe Ticks -> Posicao -> NumMinhoca -> [Minhoca] -> Maybe Ticks
calculaTempoMina mt pos dono minhocas =
    let raio = 1
        minaAtiva = any (\(i, m) -> i /= dono && eMinhocaNoRaio pos raio m) (zip [0..] minhocas)
    in
    case mt of
        Just t | t > 0 -> Just (t - 1)
        Just 0 -> Just 0
        Nothing | minaAtiva -> Just 2
        _ -> mt

-- | Avança a física específica da Dinamite (gravidade/trajetória, 'timer').
avancaFisicaDinamite :: Objeto -> Estado -> Objeto
avancaFisicaDinamite (Disparo pos dir Dinamite (Just t) dono) e
  | t <= 0 = Disparo pos dir Dinamite (Just 0) dono
  | otherwise =
      let mp     = mapaEstado e
          noChao = posChao pos mp
          tNovo = t - 1
      in if tNovo <= 0
             then Disparo pos dir Dinamite (Just 0) dono
             else if noChao
                     then Disparo pos Norte Dinamite (Just tNovo) dono
                     else let (pos', dir') = calculaTrajetoriaDinamite pos dir
                          in Disparo pos' dir' Dinamite (Just tNovo) dono
avancaFisicaDinamite o _ = o

-- | Calcula a trajetória de "arco" da Dinamite quando está no ar.
calculaTrajetoriaDinamite :: Posicao -> Direcao -> (Posicao, Direcao)
calculaTrajetoriaDinamite pos dir =
  case dir of
    Norte    -> (movePosicao Sul pos, Norte)
    Sul      -> (movePosicao Sul pos, Norte)
    Este     -> (movePosicao Sudeste pos, Sudeste)
    Nordeste -> (movePosicao Nordeste pos, Este)
    Sudeste  -> (movePosicao Sudeste pos, Sul)
    Oeste    -> (movePosicao Sudoeste pos, Sudoeste)
    Noroeste -> (movePosicao Noroeste pos, Oeste)
    Sudoeste -> (movePosicao Sudoeste pos, Sul)

-- | Retorna o terreno imediatamente abaixo de uma posição, se for válido.
terrenoAbaixo :: Posicao -> Mapa -> Maybe Terreno
terrenoAbaixo p mp =
    let posAbaixo = somaPos p (offsetDirecao Sul)
    in if posicaoInBounds posAbaixo mp
       then Just (terrenoEm posAbaixo mp)
       else Nothing

-- | Verifica se uma posição está "no chão" (i.e., sobre Terra ou Pedra).
posChao :: Posicao -> Mapa -> Bool
posChao p mp =
    case terrenoAbaixo p mp of
        Nothing    -> True
        Just Ar    -> False
        Just Agua  -> False
        Just _     -> True

-- | Obtém a posição de um objeto, se este tiver uma.
obtemPosicaoObjeto :: Objeto -> Maybe Posicao
obtemPosicaoObjeto (Disparo p _ _ _ _) = Just p
obtemPosicaoObjeto (Barril p _)         = Just p

-- | Retorna o diâmetro da explosão para um dado objeto.
obtemDiametro :: Objeto -> Int
obtemDiametro (Disparo _ _ Bazuca _ _) = 5
obtemDiametro (Disparo _ _ Mina _ _)   = 3
obtemDiametro (Disparo _ _ Dinamite _ _) = 7
obtemDiametro (Barril _ _)             = 5
obtemDiametro _                        = 0

-- | Verifica se um objeto é um Barril.
isBarril :: Objeto -> Bool
isBarril (Barril _ _) = True
isBarril _ = False

-- | Verifica se uma minhoca está dentro de um raio (quadrado) de uma posição.
eMinhocaNoRaio :: Posicao -> Int -> Minhoca -> Bool
eMinhocaNoRaio posMina raio m =
    case posicaoMinhoca m of
        Nothing -> False
        Just posWorm -> 
            let
                dl = abs (fst posMina - fst posWorm)
                dc = abs (snd posMina - snd posWorm)
            in
                dl <= raio && dc <= raio

-- | Calcula o "fator" de dano base, antes de multiplicar por 10.
calculaFatorDano :: Int -> Int -> Int -> Int
calculaFatorDano d dl dc
    | m == 0 = d 
    | dl == 0 || dc == 0 = d - 2 * m
    | otherwise          = d - 2 * m - 1
    where m = max dl dc

-- | Calcula a lista completa de 'Danos' (Posicao, Dano) para uma explosão.
calculaDanos :: Posicao -> Int -> Danos
calculaDanos posE d =
    let 
        raio = (d - 1) `div` 2
        (x, y) = posE
        
        -- Função auxiliar para mapMaybe
        calculaDanoPos :: Posicao -> Maybe (Posicao, Dano)
        calculaDanoPos pos = 
            let 
                dl = abs (fst pos - x)
                dc = abs (snd pos - y)
                fator = calculaFatorDano d dl dc
                dano = max 0 (fator * 10)
            in
                if dano > 0 then Just (pos, dano) else Nothing
        
        -- Lista de todas as posições afetadas (quadrado)
        posicoes = 
            [ (l, c) 
            | l <- [x - raio .. x + raio]
            , c <- [y - raio .. y + raio]
            ]
    
    in mapMaybe calculaDanoPos posicoes