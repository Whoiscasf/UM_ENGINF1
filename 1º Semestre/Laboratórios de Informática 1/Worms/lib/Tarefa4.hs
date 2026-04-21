{-|
Module      : Tarefa4
Description : Implementação da estratégia do agente autónomo.

Módulo para a realização da Tarefa 4 de LI1\/LP1 em 2025\/26.

-}
module Tarefa4 where

import Data.Either

import Labs2025
import Tarefa2
import Tarefa3

-- * Função Principal

-- | Função principal da Tarefa 4. 
--
-- Dado um estado inicial, executa a simulação do jogo durante 100 /ticks/.
-- Utiliza uma operação de redução (/fold/) para computar as transições de estado e agregar as jogadas resultantes.
--
tatica :: Estado -> [(NumMinhoca,Jogada)]
tatica e = reverse $ snd $ foldl simulaPassoTempo (e,[]) [0..99]

-- * Processador das Jogadas
-- | Esta secção contém as funções responsáveis pela atualização do estado do jogo,
-- permitindo ao algoritmo projetar as consequências das ações ao longo do tempo.

-- | Computa a transição de estado para um instante de tempo, agregando a nova jogada ao histórico.
simulaPassoTempo :: (Estado,[(NumMinhoca,Jogada)]) -> Ticks -> (Estado,[(NumMinhoca,Jogada)])
simulaPassoTempo (e,js) tick = (aplicarJogadaEstado j e,j:js)
    where j = determinaJogadaAtual tick e

-- | Aplica uma jogada específica ao estado atual e processa as consequências físicas.
--
-- 1. Execução da ação (movimento ou disparo).
-- 2. Atualização das minhocas (aplicação de gravidade e verificação de vitalidade).
-- 3. Atualização dos objetos (trajetórias de projéteis e resolução de explosões).
aplicarJogadaEstado :: (NumMinhoca,Jogada) -> Estado -> Estado
aplicarJogadaEstado (i,j) e@(Estado _ objetos minhocas) = foldr aplicaDanos e'' danoss''
    where
    e'@(Estado mapa' objetos' minhocas') = efetuaJogada i j e
    minhocas'' = map (atualizaMinhocaPassiva e') (zip3 [0..] minhocas minhocas')
    (objetos'',danoss'') = partitionEithers $ map (atualizaObjetoPassivo (e' { minhocasEstado = minhocas''}) objetos) (zip [0..] objetos')
    e'' = Estado mapa' objetos'' minhocas''

-- | Atualiza o estado físico de uma minhoca que não realizou a ação.
atualizaMinhocaPassiva :: Estado -> (NumMinhoca,Minhoca,Minhoca) -> Minhoca
atualizaMinhocaPassiva e (i,minhoca,minhoca') = if posicaoMinhoca minhoca == posicaoMinhoca minhoca'
    then avancaMinhoca e i minhoca'
    else minhoca'

-- | Atualiza o estado físico dos objetos, separando objetos persistentes de eventos de dano.
atualizaObjetoPassivo :: Estado -> [Objeto] -> (NumObjeto,Objeto) -> Either Objeto Danos
atualizaObjetoPassivo e objetos (i,objeto') = if elem objeto' objetos
    then avancaObjeto e i objeto'
    else Left objeto'

-- * Lógica de Decisão

-- | Seleciona a unidade ativa e determina a jogada ótima para o instante atual.
--
-- A seleção da minhoca segue um padrão sequencial, baseado no número total de minhocas.
-- Se a minhoca selecionada estiver morta, é devolvida uma jogada nula.
determinaJogadaAtual :: Ticks -> Estado -> (NumMinhoca,Jogada)
determinaJogadaAtual t e = 
    let ms = minhocasEstado e
        n = length ms
        idx = if n == 0 then 0 else t `mod` n
        w = ms !! idx
    in case vidaMinhoca w of
        Morta -> (idx, Move Norte)
        Viva _ -> (idx, calculaAcaoMinhoca w e idx t)

-- | Alias para a função de cálculo de ação (`calculaAcaoMinhoca`).
decideAcao :: Minhoca -> Estado -> Int -> Int -> Jogada
decideAcao = calculaAcaoMinhoca

-- | Lógica central de decisão.
--
--
-- 1.  __Avaliação Ofensiva__: Verifica viabilidade de disparo (`analisaDisparoBazuca`).
-- 2.  __Avaliação de Terreno__: Verifica viabilidade de escavação (`analisaEscavacao`).
-- 3.  __Movimentação__: Executa movimento padrão baseado no tempo.
calculaAcaoMinhoca :: Minhoca -> Estado -> Int -> Int -> Jogada
calculaAcaoMinhoca w e idx tick =
    case posicaoMinhoca w of
        Nothing -> Move Norte
        Just p ->
            case analisaDisparoBazuca e p idx dirs of
                Just d -> Dispara Bazuca d
                Nothing -> case analisaEscavacao e p dirs of
                    Just d -> Dispara Escavadora d
                    Nothing -> Move (dirs !! (tick `mod` 8))
    where
        dirs = [Norte, Sul, Este, Oeste, Nordeste, Noroeste, Sudeste, Sudoeste]

-- ** Algoritmos de Ataque

-- | Analisa as direções disponíveis para identificar uma oportunidade válida de disparo com Bazuca.
-- Retorna @Just Direcao@ se for encontrado um vetor de ataque válido, ou @Nothing@ caso contrário.
analisaDisparoBazuca :: Estado -> Posicao -> Int -> [Direcao] -> Maybe Direcao
analisaDisparoBazuca _ _ _ [] = Nothing
analisaDisparoBazuca e pos idx (d:ds) =
    if verificaLinhaDeTiro e pos d idx
    then Just d
    else analisaDisparoBazuca e pos idx ds

-- | Valida se a trajetória projetada numa direção está livre de obstáculos e interseta um alvo.
--
-- 
-- * Ignora células do tipo @Agua@.
-- * Interrompe a procura ao encontrar terreno opaco (@Pedra@ ou @Terra@).
-- * Retorna @True@ se intersetar uma unidade adversária ou objeto explosivo antes de encontrar um obstáculo.
verificaLinhaDeTiro :: Estado -> Posicao -> Direcao -> Int -> Bool
verificaLinhaDeTiro e pos dir idx = iterarTrajetoria (somaPos pos (offsetDirecao dir))
    where
        mp = mapaEstado e
        iterarTrajetoria p
            | not (posicaoInBounds p mp) = False 
            | eTerrenoOpaco (terrenoEm p mp) = False 
            | eAgua (terrenoEm p mp) = iterarTrajetoria (somaPos p (offsetDirecao dir))
            | existeAlvoNaPosicao e p idx = True
            | otherwise = iterarTrajetoria (somaPos p (offsetDirecao dir))

-- | Verifica a existência de um alvo válido numa coordenada específica.
--
-- Critérios de alvo válido:
--
-- * __Inimigos__: Minhocas com índice diferente da minhoca atual e vivas.
--
-- * __Barris__: Objetos estáticos possiveis de explodir.
existeAlvoNaPosicao :: Estado -> Posicao -> Int -> Bool
existeAlvoNaPosicao e p idx = detectaInimigo p (minhocasEstado e) 0 || detectaBarril p (objetosEstado e)
    where
        detectaInimigo _ [] _ = False
        detectaInimigo pos (m:ms) i =
            (i /= idx && posicaoMinhoca m == Just pos && vidaMinhoca m /= Morta) || detectaInimigo pos ms (i + 1)

        detectaBarril _ [] = False
        detectaBarril pos (o:os) = case o of
            Barril pb _ -> pb == pos || detectaBarril pos os
            _ -> detectaBarril pos os

-- ** Algoritmos de Modificação de Terreno

-- | Analisa as direções adjacentes para determinar a viabilidade de utilização da Escavadora.
-- Prioriza a remoção de obstáculos para criação de rotas de fuga ou acesso.
analisaEscavacao :: Estado -> Posicao -> [Direcao] -> Maybe Direcao
analisaEscavacao e pos = identificaTerraAdjacente e pos

-- | Percorre recursivamente a lista de direções fornecida, retornando a primeira direção 
-- que conduza a uma posição válida contendo terreno do tipo @Terra@.
identificaTerraAdjacente :: Estado -> Posicao -> [Direcao] -> Maybe Direcao
identificaTerraAdjacente _ _ [] = Nothing
identificaTerraAdjacente e pos (d:ds) =
    let alvo = somaPos pos (offsetDirecao d)
        mp = mapaEstado e
    in if posicaoInBounds alvo mp && terrenoEm alvo mp == Terra
       then Just d
       else identificaTerraAdjacente e pos ds

-- * Funções Auxiliares Gerais

-- | Predicado que indica se um terreno bloqueia a linha de visão e movimento (Opaco).
eTerrenoOpaco :: Terreno -> Bool
eTerrenoOpaco Terra = True
eTerrenoOpaco Pedra = True
eTerrenoOpaco _     = False

-- | Predicado que indica se um terreno é do tipo Água.
eAgua :: Terreno -> Bool
eAgua Agua = True
eAgua _    = False