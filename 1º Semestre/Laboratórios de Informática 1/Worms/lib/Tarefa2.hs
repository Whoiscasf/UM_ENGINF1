{-|
Module      : Tarefa2
Description : Efetuar jogadas (movimento e disparos).

Módulo para a realização da Tarefa 2 de LI1\/LP1 em 2025\/26.
-}

module Tarefa2 where
import Labs2025
import Tarefa0_2025

-- | Calcula o 'offset' (delta linha, delta coluna) correspondente a uma 'Direcao'.
offsetDirecao :: Direcao -> (Int,Int)
offsetDirecao d = case d of
    Norte     -> (-1,  0); Sul       -> ( 1,  0)
    Este      -> ( 0,  1); Oeste     -> ( 0, -1)
    Nordeste  -> (-1,  1); Noroeste  -> (-1, -1)
    Sudeste   -> ( 1,  1); Sudoeste  -> ( 1, -1)

-- | Adiciona um 'offset' (delta linha, delta coluna) a uma 'Posicao'.
somaPos :: Posicao -> (Int,Int) -> Posicao
somaPos (l,c) (dl,dc) = (l+dl, c+dc)

-- Funções auxiliares de lista
-- | Atualiza a i-ésima minhoca na lista.
atualizaMinhoca :: Int -> (Minhoca -> Minhoca) -> [Minhoca] -> [Minhoca]
atualizaMinhoca i f ms
    | i < 0 || i >= length ms = ms
    | otherwise =
        let (antes, resto) = splitAt i ms
        in case resto of
             (x:depois) -> antes ++ (f x) : depois
             []         -> ms

-- | Pega a i-ésima minhoca (0-based). Retorna 'Nothing' se o índice for inválido.
pegaMinhoca :: Int -> [Minhoca] -> Maybe Minhoca
pegaMinhoca i ms
    | i < 0 || i >= length ms = Nothing
    | otherwise = Just (ms !! i)

-- Funções auxiliares de estado da minhoca
-- | Verifica se uma 'Minhoca' está viva (tem 'VidaMinhoca' do tipo 'Viva').
minhocaViva :: Minhoca -> Bool
minhocaViva m = case vidaMinhoca m of
    Viva _ -> True
    Morta  -> False

-- | Define o estado de uma 'Minhoca' como 'Morta', atualizando a sua 'posicaoMinhoca'.
matarMinhoca :: Minhoca -> Maybe Posicao -> Minhoca
matarMinhoca m mp = m { vidaMinhoca = Morta, posicaoMinhoca = mp }

-- | Função auxiliar para atualizar a 'posicaoMinhoca' e a 'vidaMinhoca' de uma 'Minhoca' em simultâneo.
atualizaPosVidaMinhoca :: Minhoca -> Maybe Posicao -> VidaMinhoca -> Minhoca
atualizaPosVidaMinhoca m p v = m { posicaoMinhoca = p, vidaMinhoca = v }

-- Funções auxiliares de mapa
-- | Verifica se uma 'Posicao' (linha, coluna) está dentro dos limites válidos do 'Mapa'.
posicaoInBounds :: Posicao -> Mapa -> Bool
posicaoInBounds (r,c) m =
    not (null m) && not (null (head m)) &&
    r >= 0 && r < length m &&
    c >= 0 && c < length (head m)

-- | Obtém o 'Terreno' numa dada 'Posicao' do 'Mapa'.
terrenoEm :: Posicao -> Mapa -> Terreno
terrenoEm (r,c) m = (m !! r) !! c

-- | Define a situação de uma minhoca em relação ao terreno imediatamente abaixo dela.
data SituacaoChao = NoAr | NaAgua | NoChao deriving (Eq,Show)

-- | Determina a 'SituacaoChao' de uma minhoca.
--   Minhocas sem posição ou na borda inferior são consideradas 'NoChao'.
situacaoMinhoca :: Minhoca -> Estado -> SituacaoChao
situacaoMinhoca m e = case posicaoMinhoca m of
    Nothing -> NoChao
    Just p ->
        let abaixo = somaPos p (1,0)
            mp = mapaEstado e
        in
        if not (posicaoInBounds abaixo mp)
           then NoChao
           else case terrenoEm abaixo mp of
                 Ar   -> NoAr
                 Agua -> NaAgua
                 _    -> NoChao

-- | Função central de movimento. Tenta mover uma minhoca para um destino,
--   aplicando as consequências físicas.
--   Recebe a minhoca no seu estado final.
tentaMoverMinhoca :: NumMinhoca -> Minhoca -> Posicao -> Estado -> Estado
tentaMoverMinhoca n minhocaAtualizada dest e =
    let mp = mapaEstado e
    in
    if not (posicaoInBounds dest mp)
    then -- Fora do mapa -> morre, sem posição
         e { minhocasEstado = atualizaMinhoca n (\_ -> matarMinhoca minhocaAtualizada Nothing) (minhocasEstado e) }
    else if not (ePosicaoEstadoLivre dest e)
    then -- Ocupado (Pedra, Terra, Minhoca, Barril) -> não move, mas atualiza estado (ex: gasta munição)
         e { minhocasEstado = atualizaMinhoca n (\_ -> minhocaAtualizada) (minhocasEstado e) }
    else if terrenoEm dest mp == Agua
    then -- Água -> morre, na nova posição
         e { minhocasEstado = atualizaMinhoca n (\_ -> matarMinhoca minhocaAtualizada (Just dest)) (minhocasEstado e) }
    else -- Ar livre -> move com sucesso
         e { minhocasEstado = atualizaMinhoca n (\_ -> minhocaAtualizada { posicaoMinhoca = Just dest }) (minhocasEstado e) }


-- | Função principal que processa uma 'Jogada' (Move ou Dispara) para uma dada 'Minhoca'.
--   Valida se a minhoca existe antes de delegar para 'efetuaMovimento' ou 'efetuaDisparo'.
efetuaJogada :: NumMinhoca -> Jogada -> Estado -> Estado
efetuaJogada n jogada estado =
    case pegaMinhoca n (minhocasEstado estado) of
        Nothing -> estado
        Just minh ->
            case jogada of
                Move dir -> efetuaMovimento n dir minh estado
                Dispara tipo dir -> efetuaDisparo n tipo dir minh estado

-- | Processa uma jogada 'Move'. 
--   Verifica as restrições de jogo (ex: viva, 'SituacaoChao') antes de tentar o movimento físico com 'tentaMoverMinhoca'.
efetuaMovimento :: NumMinhoca -> Direcao -> Minhoca -> Estado -> Estado
efetuaMovimento n dir m e
    | not (minhocaViva m) = e
    | otherwise =
        case posicaoMinhoca m of
            Nothing -> e -- Não pode mover-se sem posição
            Just p  -> -- Tem posição, verifica a lógica de jogo
                let sit = situacaoMinhoca m e
                    ehSubida = case dir of { Norte -> True; Nordeste -> True; Noroeste -> True; _ -> False }
                in
                case sit of
                    NoAr -> e
                    NaAgua | not ehSubida -> e
                    _      | ehSubida && sit /= NoChao -> e
                    -- Lógica de jogo válida, aplica o movimento físico
                    _      -> tentaMoverMinhoca n m (somaPos p (offsetDirecao dir)) e

-- | Tenta criar um 'Objeto' do tipo 'Disparo' (Bazuca, Mina, Dinamite) com base na 'Posicao' e 'Direcao' de origem. 
--   Retorna 'Nothing' se o disparo for inválido.
--   Aplica regras para Mina e Dinamite se a célula de destino estiver ocupada.
criaObjetoDisparo :: TipoArma -> Direcao -> Posicao -> NumMinhoca -> Estado -> Maybe Objeto
criaObjetoDisparo tipo dir posOrig dono e =
    let dest = somaPos posOrig (offsetDirecao dir)
        mp = mapaEstado e
    in case tipo of
        Bazuca ->
            if posicaoInBounds dest mp
            then Just (Disparo dest dir Bazuca Nothing dono)
            else Nothing 

        Mina ->
            if not (posicaoInBounds dest mp)
            then Nothing
            else Just (Disparo (if ePosicaoEstadoLivre dest e then dest else posOrig) dir Mina Nothing dono)

        Dinamite ->
            if not (posicaoInBounds dest mp)
            then Nothing 
            else Just (Disparo (if ePosicaoEstadoLivre dest e then dest else posOrig) dir Dinamite (Just 4) dono)

        _ -> Nothing -- Não aplicável


-- | Processa uma jogada do tipo 'Dispara'. 
--   Verifica as restrições, gasta a munição 
--   e aplica o efeito da arma
efetuaDisparo :: NumMinhoca -> TipoArma -> Direcao -> Minhoca -> Estado -> Estado
efetuaDisparo n tipo dir m e
    | not (minhocaViva m) = e
    | encontraQuantidadeArmaMinhoca tipo m <= 0 = e
    | minhocaTemDisparo tipo n (objetosEstado e) = e
    | otherwise =
        case posicaoMinhoca m of
            Nothing -> e
            Just p ->
                -- A munição é sempre gasta.
                let gastaMun = atualizaQuantidadeArmaMinhoca tipo m (encontraQuantidadeArmaMinhoca tipo m - 1)
                    estadoSoMun = e { minhocasEstado = atualizaMinhoca n (\_ -> gastaMun) (minhocasEstado e) }
                in
                case tipo of
                    Jetpack ->
                        let dest = somaPos p (offsetDirecao dir)
                        in tentaMoverMinhoca n gastaMun dest e

                    Escavadora ->
                        let dest = somaPos p (offsetDirecao dir)
                            mp = mapaEstado e
                        in
                        if not (posicaoInBounds dest mp)
                        then tentaMoverMinhoca n gastaMun dest e -- Deixa 'tentaMoverMinhoca' tratar da morte
                        else
                            let terr = terrenoEm dest mp
                            in case terr of
                                Terra ->
                                    let ocupadoMinhocaOuBarril =
                                           any (\m' -> posicaoMinhoca m' == Just dest) (minhocasEstado e)
                                           || any (\o -> case o of Barril pos _ -> pos == dest; _ -> False) (objetosEstado e)
                                    in
                                    if ocupadoMinhocaOuBarril
                                    then estadoSoMun -- Ocupado, só gasta munição
                                    else -- Livre, escava e move
                                        let mapaNovo = destroiPosicao dest mp
                                            estadoInter = e { mapaEstado = mapaNovo }
                                        in
                                        tentaMoverMinhoca n gastaMun dest estadoInter
                                Ar ->
                                     tentaMoverMinhoca n gastaMun dest e -- Move-se para Ar (se livre)
                                
                                _ -> estadoSoMun -- Pedra, Agua, etc. -> Só gasta munição

                    -- Lógica de Bazuca, Mina e Dinamite
                    _ -> case criaObjetoDisparo tipo dir p n e of
                            Nothing  -> estadoSoMun -- Disparo inválido (ex: fora do mapa)
                            Just obj -> estadoSoMun { objetosEstado = obj : objetosEstado e }