{-|
Module      : Tarefa0_geral
Description : Funções auxiliares gerais.

Módulo que define funções genéricas sobre listas e matrizes.
-}

module Tarefa0_geral where

-- * Tipos de dados

-- | Uma matriz é um conjunto de elementos a duas dimensões.
--
-- Em notação matemática, é geralmente representada por:
--
-- <<https://haslab.github.io/Teaching/LI1/2526/img/matriz.png>>
type Matriz a = [[a]]

-- | Uma posição numa matriz é dada como um par (/linha/,/colunha/).
-- As coordenadas são dois números naturais e começam com (0,0) no canto superior esquerdo, com as linhas incrementando para baixo e as colunas incrementando para a direita.
--
-- <<https://haslab.github.io/Teaching/LI1/2526/img/posicaomatriz.png>>
type Posicao = (Int,Int)

-- | A dimensão de uma matrix dada como um par (/número de linhas/,/número de colunhas/).
type Dimensao = (Int,Int)

-- | Uma direção é dada pela rosa dos ventos. Ou seja, os 4 pontos cardeais e os 4 pontos colaterais.
--
-- <<https://haslab.github.io/Teaching/LI1/2526/img/rosadosventos.jpg>>
data Direcao = Norte | Nordeste | Este | Sudeste | Sul | Sudoeste | Oeste | Noroeste
    deriving (Eq,Ord,Show,Read,Enum)

-- * Funções não-recursivas.

-- | Verifica se o indice pertence à lista.
eIndiceListaValido :: Int -> [a] -> Bool
eIndiceListaValido i xs = i >= 0 && i < length xs


-- | Calcula a dimensão de uma matriz.
--
-- __NB:__ Note que não existem matrizes de dimensão /m * 0/ ou /0 * n/, e que qualquer matriz vazia deve ter dimensão /0 * 0/.
dimensaoMatriz :: Matriz a -> Dimensao
dimensaoMatriz m = (length m, if null m then 0 else length (head m))


-- | Verifica se a posição pertence à matriz.
ePosicaoMatrizValida :: Posicao -> Matriz a -> Bool 
ePosicaoMatrizValida (x,y) m = let (l,c) = dimensaoMatriz m in x >= 0 && x < l && y >= 0 && y < c


-- | Move uma posição uma unidade no sentido de uma direção.
movePosicao :: Direcao -> Posicao -> Posicao
movePosicao dir (x,y) = case dir of
    Norte     -> (x-1, y)
    Nordeste  -> (x-1, y+1)
    Este      -> (x,   y+1)
    Sudeste   -> (x+1, y+1)
    Sul       -> (x+1, y)
    Sudoeste  -> (x+1, y-1)
    Oeste     -> (x,   y-1)
    Noroeste  -> (x-1, y-1)

-- | Versão da função 'movePosicao' que garante que o movimento não se desloca para fora de uma janela.
--
-- __NB:__ Considere uma janela retangular com origem no canto superior esquerdo definida como uma matriz. A função recebe a dimensao da janela.
movePosicaoJanela :: Dimensao -> Direcao -> Posicao -> Posicao
movePosicaoJanela (l,c) dir pos =
    let (nx, ny) = movePosicao dir pos
    -- Verifica se a nova posição (nx, ny) está dentro dos limites (0..l-1, 0..c-1)
    in if nx >= 0 && nx < l && ny >= 0 && ny < c
       then (nx, ny)
       else pos

-- | Converte uma posição no referencial em que a origem é no canto superior esquerdo da janela numa posição em que a origem passa a estar no centro da janela.
--
-- __NB:__ Considere posições válidas. Efetue arredondamentos como achar necessário.
origemAoCentro :: Dimensao -> Posicao -> Posicao
origemAoCentro (l,c) (x,y) =
    let centroX = (l - 1) `div` 2
        centroY = (c - 1) `div` 2
    in (x - centroX, y - centroY)

-- | Roda um par (posição,direção) 45% para a direita.
--
-- __NB:__ Vendo um par (posição,direção) como um vector, cria um novo vetor do desto com a próxima direção da rosa dos ventos rodando para a direita.
--
-- <<https://haslab.github.io/Teaching/LI1/2526/img/rodaposicaodirecao.png>>
rodaPosicaoDirecao :: (Posicao,Direcao) -> (Posicao,Direcao)
rodaPosicaoDirecao (pos, dir) =
    let novaDir = case dir of
            Norte     -> Nordeste
            Nordeste  -> Este
            Este      -> Sudeste
            Sudeste   -> Sul
            Sul       -> Sudoeste
            Sudoeste  -> Oeste
            Oeste     -> Noroeste
            Noroeste  -> Norte
    in (pos, novaDir)

-- * Funções recursivas.

-- | Devolve o elemento num dado índice de uma lista.
--
-- __NB:__ Retorna @Nothing@ se o índice não existir.
encontraIndiceLista :: Int -> [a] -> Maybe a
encontraIndiceLista i xs
    | not (eIndiceListaValido i xs) = Nothing
    | otherwise = Just (xs !! i)

-- | Modifica um elemento num dado índice. (Versão recursiva)
--
-- __NB:__ Devolve a própria lista se o elemento não existir.
atualizaIndiceLista :: Int -> a -> [a] -> [a]
atualizaIndiceLista i _ xs
    | not (eIndiceListaValido i xs) = xs
-- Casos recursivos
atualizaIndiceLista _ _ [] = [] -- Caso base (embora protegido por eIndiceListaValido)
atualizaIndiceLista i v (x:xs)
    | i == 0    = v : xs
    | otherwise = x : atualizaIndiceLista (i - 1) v xs

-- | Devolve o elemento numa dada posição de uma matriz.
--
-- __NB:__ Retorna @Nothing@ se a posição não existir.
encontraPosicaoMatriz :: Posicao -> Matriz a -> Maybe a
encontraPosicaoMatriz (x,y) m
    | not (ePosicaoMatrizValida (x,y) m) = Nothing
    | otherwise = Just ((m !! x) !! y)

-- | Modifica um elemento numa dada posição de uma matriz. (Versão recursiva)
--
-- __NB:__ Devolve a própria matriz se o elemento não existir.
atualizaPosicaoMatriz :: Posicao -> a -> Matriz a -> Matriz a
atualizaPosicaoMatriz (x,y) _ m
    | not (ePosicaoMatrizValida (x,y) m) = m
-- Casos recursivos
atualizaPosicaoMatriz _ _ [] = [] -- Caso base 
atualizaPosicaoMatriz (x,y) v (l:ls)
    | x == 0    = (atualizaIndiceLista y v l) : ls
    | otherwise = l : atualizaPosicaoMatriz (x - 1, y) v ls

-- | Aplica uma sequência de movimentações a uma posição, pela ordem em que ocorrem na lista.
moveDirecoesPosicao :: [Direcao] -> Posicao -> Posicao
moveDirecoesPosicao [] pos = pos
moveDirecoesPosicao (d:ds) pos = moveDirecoesPosicao ds (movePosicao d pos)

-- | Aplica a mesma movimentação a uma lista de posições.
moveDirecaoPosicoes :: Direcao -> [Posicao] -> [Posicao]
moveDirecaoPosicoes _ [] = []
moveDirecaoPosicoes dir (p:ps) = movePosicao dir p : moveDirecaoPosicoes dir ps

-- | Função auxiliar recursiva para eMatrizValida.
--   Verifica se todas as listas em 'linhas' têm a 'larguraEsperada'.
verificaLinhas :: [[a]] -> Int -> Bool
verificaLinhas [] _ = True 
verificaLinhas (linha:linhas) larguraEsperada =
    if length linha == larguraEsperada
    then verificaLinhas linhas larguraEsperada
    else False

-- | Verifica se uma matriz é válida, no sentido em que modela um rectângulo. (Versão recursiva)
--
-- __NB:__ Todas as linhas devem ter o mesmo número de colunas. 
eMatrizValida :: Matriz a -> Bool
eMatrizValida [] = True -- Matriz 0x0 é válida
eMatrizValida (x:xs) =
    if null x
    then False -- Matriz m*0 (com m>0) é inválida 
    else verificaLinhas xs (length x)