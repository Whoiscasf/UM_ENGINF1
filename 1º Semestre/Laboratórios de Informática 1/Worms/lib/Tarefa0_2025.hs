{-|
Module      : Tarefa0_2025
Description : Funções auxiliares.

Módulo que define funções auxiliares que serão úteis na resolução do trabalho prático de LI1\/LP1 em 2025\/26.
-}

-- | Este módulo 
module Tarefa0_2025 where
    
import Labs2025

-- | Retorna a quantidade de munições disponíveis de uma minhoca para uma dada arma.
-- | Devolve a quantidade de munições de uma dada arma para uma minhoca.
encontraQuantidadeArmaMinhoca :: TipoArma -> Minhoca -> Int
encontraQuantidadeArmaMinhoca t m = case t of
    Jetpack    -> jetpackMinhoca m
    Bazuca     -> bazucaMinhoca m
    Escavadora -> escavadoraMinhoca m
    Dinamite   -> dinamiteMinhoca m
    Mina       -> minaMinhoca m

-- | Atualiza a quantidade de munições disponíveis de uma minhoca para uma dada arma.
atualizaQuantidadeArmaMinhoca :: TipoArma -> Minhoca -> Int -> Minhoca
atualizaQuantidadeArmaMinhoca t m i = case t of
    Jetpack    -> m { jetpackMinhoca = i }
    Bazuca     -> m { bazucaMinhoca = i }
    Escavadora -> m { escavadoraMinhoca = i }
    Dinamite   -> m { dinamiteMinhoca = i }
    Mina       -> m { minaMinhoca = i }

-- | Verifica se um tipo de terreno é destrutível, i.e., pode ser destruído por explosões.
--
-- __NB:__ Apenas @Terra@ é destrutível.
eTerrenoDestrutivel :: Terreno -> Bool
eTerrenoDestrutivel Terra = True
eTerrenoDestrutivel _     = False


-- | Verifica se um tipo de terreno é opaco, i.e., não permite que objetos ou minhocas se encontrem por cima dele.
--
-- __NB:__ Apenas @Terra@ ou @Pedra@ são opacos.
eTerrenoOpaco :: Terreno -> Bool
eTerrenoOpaco Terra = True
eTerrenoOpaco Pedra = True
eTerrenoOpaco _     = False


-- | Verifica se uma posição do mapa está livre, i.e., pode ser ocupada por um objeto ou minhoca.
--
-- __NB:__ Uma posição está livre se não contiver um terreno opaco.
ePosicaoMapaLivre :: Posicao -> Mapa -> Bool
ePosicaoMapaLivre (p1, p2) m
    -- Verifica se o mapa ou a primeira linha estão vazios
    | null m || null (head m) = False
    | otherwise =
        let altura = length m
            largura = length (head m)
            -- Verifica se a posição está dentro dos limites do mapa
            inBounds = p1 >= 0 && p1 < altura && p2 >= 0 && p2 < largura
        -- A posição está livre SE estiver dentro dos limites E o terreno não for opaco
        in inBounds && not (eTerrenoOpaco (m !! p1 !! p2))

-- | Verifica se uma posição do estado está livre, i.e., pode ser ocupada por um objeto ou minhoca.
--
-- __NB:__ Uma posição está livre se o mapa estiver livre e se não estiver já uma minhoca ou um barril nessa posição.
ePosicaoEstadoLivre :: Posicao -> Estado -> Bool
ePosicaoEstadoLivre pos e =
    ePosicaoMapaLivre pos (mapaEstado e) &&
    not (any (minhocaNaPosicao pos) (minhocasEstado e)) &&
    not (any (barrilNaPosicao pos) (objetosEstado e))
  where
    -- Predicado para 'any': verifica se a minhoca 'm' está na posição 'p'
    minhocaNaPosicao p m = posicaoMinhoca m == Just p
    
    -- Predicado para 'any': verifica se o objeto 'obj' é um barril na posição 'p'
    -- (Assume que Objeto tem um construtor Barril)
    barrilNaPosicao p (Barril {posicaoBarril = posB}) = p == posB
    barrilNaPosicao _ _                               = False -- Ignora outros tipos de Objeto (ex: Disparo)

-- | Verifica se numa lista de objetos já existe um disparo feito para uma dada arma por uma dada minhoca.
minhocaTemDisparo :: TipoArma -> NumMinhoca -> [Objeto] -> Bool
minhocaTemDisparo arma num = any verificaDisparo
  where
    verificaDisparo Disparo{tipoDisparo = tipo, donoDisparo = dono} =
      tipo == arma && dono == num
    verificaDisparo _ = False

-- | Destrói uma dada posição no mapa (tipicamente como consequência de uma explosão).
--
-- __NB__: Só terrenos @Terra@ pode ser destruídos.
destroiPosicao :: Posicao -> Mapa -> Mapa
destroiPosicao (p1, p2) m =
    case splitAt p1 m of
        (linhasAntes, linhaAtual : linhasDepois) ->
            -- Se p1 for um índice válido, temos uma linhaAtual. Agora verifica p2.
            case splitAt p2 linhaAtual of
                (colunasAntes, terrenoAtual : colunasDepois) ->
                    -- Se p2 for um índice válido, temos um terrenoAtual.
                    -- Verifica se é destrutível
                    if eTerrenoDestrutivel terrenoAtual
                    then -- Reconstrói a linha com 'Ar'
                         let novaLinha = colunasAntes ++ [Ar] ++ colunasDepois
                         -- Reconstrói o mapa
                         in linhasAntes ++ [novaLinha] ++ linhasDepois
                    else -- Posição (p1, p2) existe mas não é destrutível, retorna mapa original
                         m
                _ -> m -- p2 está fora dos limites, retorna mapa original
        _ -> m -- p1 está fora dos limites, retorna mapa original

-- | Adiciona um novo objeto a um estado.
--
-- __NB__: A posição onde é inserido não é relevante.
adicionaObjeto :: Objeto -> Estado -> Estado
adicionaObjeto obj e = e { objetosEstado = obj : objetosEstado e }