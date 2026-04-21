{-|
Module      : Tarefa1
Description : Validação de estados.

Módulo para a realização da Tarefa 1 de LI1\/LP1 em 2025\/26.
-}
module Tarefa1 where

import Labs2025

-- | Função principal da Tarefa 1. Recebe um estado e retorna se este é válido ou não.
--   Agrega as validações do mapa ('validaMapa'), dos objetos ('validaObjetos')
--   e das minhocas ('validaMinhocas').
validaEstado :: Estado -> Bool
validaEstado e = 
        validaMapa (mapaEstado e)
    && validaObjetos e 
    && validaMinhocas e 
            
    
-- | Verifica se um 'Mapa' é válido.
-- Um mapa é válido se não for vazio ('mapaNaoVazio'),
-- for retangular ('mapaRetangular') e
-- todos os seus 'Terreno's forem válidos.
validaMapa :: Mapa -> Bool
validaMapa m = 
    mapaNaoVazio m
    && mapaRetangular m 
    && mapaTerrenosValidos m

-- | Verifica se um 'Mapa' não é vazio.
mapaNaoVazio :: Mapa -> Bool
mapaNaoVazio m = not (null m)

-- | Verifica se um 'Mapa' é retangular.
-- Garante que o mapa não é vazio, a primeira linha não é vazia
-- e que todas as linhas têm o mesmo comprimento que a primeira.
mapaRetangular :: Mapa -> Bool
mapaRetangular []      = False
mapaRetangular (r:rs) = not (null r) && all ((==length r) . length) rs

-- | Verifica se todos os 'Terreno's num 'Mapa' são válidos, usando 'terrenoValido'.
mapaTerrenosValidos :: Mapa -> Bool
mapaTerrenosValidos m = all (all terrenoValido) m

-- | Verifica se um 'Terreno' é um dos tipos permitidos ('Ar', 'Agua', 'Terra', 'Pedra').
terrenoValido :: Terreno -> Bool
terrenoValido t = t `elem` [Ar,Agua,Terra,Pedra] 

-- | Define se um 'Terreno' é opaco (bloqueia movimento/disparos).
-- 'Terra' e 'Pedra' são opacos.
opaco :: Terreno -> Bool
opaco Terra = True 
opaco Pedra = True 
opaco _ = False   

-- | Verifica se um 'Terreno' é 'Agua'.
eAgua :: Terreno -> Bool
eAgua Agua = True
eAgua _    = False

-- | Retorna o 'Terreno' numa dada 'Posicao' (linha, coluna) do 'Mapa'.
--   Assume que a posição é válida (não verifica limites).
cel :: Mapa -> Posicao -> Terreno
cel m (i,j) = (m !! i) !! j

-- | Verifica se uma 'Posicao' (linha, coluna) está dentro dos limites do 'Mapa'.
dentroMapa :: Mapa -> Posicao -> Bool 
dentroMapa m (i,j) = 
    let nl = length m 
        nc = if null m then 0 else length (head m)
    in 0 <= i && i < nl && 0 <= j && j < nc 

-- | Verifica se uma célula numa 'Posicao' é "livre".
-- Uma célula é livre se está 'dentroMapa' e o seu 'Terreno' não é 'opaco'.
livre :: Mapa -> Posicao -> Bool
livre m p = dentroMapa m p && not (opaco (cel m p))

-- | Calcula a 'Posicao' adjacente "anterior" (de onde veio) a uma dada 'Posicao',
-- com base numa 'Direcao' de movimento. Usado para a exceção da 'Bazuca'.
posAnterior :: Posicao -> Direcao -> Posicao
posAnterior (i,j) d = case d of
    Norte      -> (i+1,j)
    Sul        -> (i-1,j)
    Este       -> (i,j-1)
    Oeste      -> (i,j+1)
    Nordeste   -> (i+1, j-1)
    Sudeste    -> (i-1, j-1)
    Sudoeste   -> (i-1, j+1)
    Noroeste   -> (i+1, j+1)  

-- | Verifica se um 'TipoArma' é uma arma que gera um 'Objeto' do tipo 'Disparo'.
-- Rejeita armas de efeito instantâneo ('Jetpack', 'Escavadora').
armaDeDisparoValida :: TipoArma -> Bool
armaDeDisparoValida a = a `elem` [Mina, Dinamite, Bazuca]

-- | Valida o campo de tempo ('Maybe Int') de um 'Disparo' com base no 'TipoArma'.
-- 'Mina': 'Nothing' ou 'Just' t>=0.
-- 'Dinamite': 'Just' t>=0 (obrigatório).
-- 'Bazuca': 'Nothing' (obrigatório).
tempoValido :: TipoArma -> Maybe Int -> Bool
tempoValido Mina     mt = maybe True  (>=0) mt     -- Mina: aceita Nothing ou Just t>=0
tempoValido Dinamite mt = maybe False (>=0) mt     -- Dinamite: obrigatório Just t>=0
tempoValido Bazuca   mt = mt == Nothing            -- Bazuca: sem tempo
tempoValido _        _  = False                   -- Jetpack/Escavadora não são Disparos válidos

-- | Verifica a regra de posição normal para a criação de um 'Disparo'.
-- A 'Posicao' deve estar 'dentroMapa' e 'livre'.
-- Não trata a exceção da 'Bazuca'.
celulaOkParaDisparo :: Mapa -> TipoArma -> Posicao -> Bool
celulaOkParaDisparo m a p =
  dentroMapa m p &&
  case a of
    Bazuca   -> livre m p
    Mina     -> livre m p      
    Dinamite -> livre m p      
    _        -> False          -- Jetpack/Escavadora não são Disparos válidos

-- | Verifica se a posição de um 'Objeto' ('Barril' ou 'Disparo') é válida no 'Mapa'.
-- Para 'Barril': deve estar numa 'Posicao' 'livre'.
-- Para 'Disparo': valida 'armaDeDisparoValida', 'tempoValido', e a 'Posicao'
-- (incluindo a regra normal 'celulaOkParaDisparo' e a exceção da 'Bazuca').
posicaoValidaObjeto :: Mapa -> Objeto -> Bool
posicaoValidaObjeto m (Barril p _) =
  dentroMapa m p && livre m p

posicaoValidaObjeto m (Disparo p dir arma mt _ ) =
  armaDeDisparoValida arma                  -- rejeita armas instantâneas
  && tempoValido arma mt                    -- valida tempo por tipo
  && (
        -- regra normal da célula
        celulaOkParaDisparo m arma p
        -- exceção da Bazuca: pode estar em opaco se atrás for não-opaco
     || ( arma == Bazuca
        && dentroMapa m p
        && opaco (cel m p)
        && let pPrev = posAnterior p dir
           in dentroMapa m pPrev && not (opaco (cel m pPrev))
        )
     )

-- | Extrai uma lista de 'Posicao' de todos os 'Barril's numa lista de 'Objeto's.
posicoesBarris :: [Objeto] -> [Posicao]
posicoesBarris objs = [ p | Barril p _ <- objs ]

-- | Verifica se não existem 'Barril's sobrepostos (em posições duplicadas).
barrisSemSobreposicao :: [Objeto] -> Bool
barrisSemSobreposicao objs =
    barrisSemRepetidos (posicoesBarris objs)

-- | Função auxiliar genérica para verificar se uma lista não contém elementos repetidos.
barrisSemRepetidos :: Eq a => [a] -> Bool
barrisSemRepetidos [] = True
barrisSemRepetidos (x:xs) = not (x `elem` xs) && barrisSemRepetidos xs    

-- | Verifica se nenhum 'Barril' está na mesma 'Posicao' que uma 'Minhoca'.
barrisForaDeMinhocas :: [Objeto] -> [Minhoca] -> Bool
barrisForaDeMinhocas objs ms =
    let psB = posicoesBarris objs
        psM = posicoesMinhocas ms
    in null [p | p <- psB, p `elem` psM]

-- | Extrai uma lista de 'Posicao' de todos os 'Disparo's do tipo 'Mina' ou 'Dinamite'.
posicoesMinasEDinamites :: [Objeto] -> [Posicao]
posicoesMinasEDinamites objs =
  [ posicaoDisparo d
  | d@(Disparo {}) <- objs
  , tipoDisparo d `elem` [Mina, Dinamite]
  ]

-- | Verifica se nenhuma 'Mina' ou 'Dinamite' está na mesma 'Posicao' que um 'Barril'.
minasEDinamitesForaDeBarris :: [Objeto] -> Bool
minasEDinamitesForaDeBarris objs =
    let psB = posicoesBarris objs
        psM = posicoesMinasEDinamites objs
    in null [p | p <- psM, p `elem` psB]

-- | Verifica todas as regras de validação relativas a 'Barril's:
-- Não sobreposição ('barrisSemSobreposicao').
-- Não estar sobre 'Minhoca's ('barrisForaDeMinhocas').
-- 'Mina's e 'Dinamite's não estarem sobre 'Barril's ('minasEDinamitesForaDeBarris').
validaBarris :: Estado -> Bool
validaBarris e =
    let objs = objetosEstado e
        ms   = minhocasEstado e
    in  barrisSemSobreposicao objs
     && barrisForaDeMinhocas objs ms
     && minasEDinamitesForaDeBarris objs

-- | Verifica se o 'dono' (índice da 'Minhoca') de um 'Disparo' é válido.
-- O índice deve estar entre 0 e (número de minhocas - 1).
-- Retorna 'True' para 'Objeto's que não são 'Disparo'.
donoDisparoValido :: Estado -> Objeto -> Bool
donoDisparoValido e (Disparo _ _ _ _ dono) =
  let n = length (minhocasEstado e)
  in  0 <= dono && dono < n
donoDisparoValido _ _ = True

-- | Verifica se a lista de 'Objeto's de um 'Estado' é válida.
-- Cada objeto deve ter uma posição válida ('posicaoValidaObjeto').
-- Cada 'Disparo' deve ter um 'dono' válido ('donoDisparoValido').
-- As regras de 'Barril's devem ser compridas ('validaBarris').
validaObjetos :: Estado -> Bool
validaObjetos e =
  let m  = mapaEstado e
      os = objetosEstado e
  in  all (posicaoValidaObjeto m) os
   && all (donoDisparoValido e) os
   && validaBarris e

-- | Verifica se a 'Posicao' de uma 'Minhoca' é válida.
-- Se a minhoca não tiver posição ('Nothing'), é válido.
-- Se tiver ('Just p'), 'p' deve ser 'dentroMapa' e 'livre'.
minhocaPosicaoValida :: Mapa -> Minhoca -> Bool
minhocaPosicaoValida m w =
  case posicaoMinhoca w of
    Nothing -> True
    Just p  -> dentroMapa m p && livre m p

-- | Extrai uma lista de 'Posicao' de todas as 'Minhoca's que têm uma posição definida ('Just p').
posicoesMinhocas :: [Minhoca] -> [Posicao]
posicoesMinhocas ms = [p | Just p <- map posicaoMinhoca ms]

-- | Verifica se não existem 'Minhoca's sobrepostas (em posições duplicadas).
semSobreposicaoMinhocas :: [Minhoca] -> Bool
semSobreposicaoMinhocas ms =
  let ps = posicoesMinhocas ms
  in minhocasSemRepetidos ps

-- | Função auxiliar genérica para verificar se uma lista não contém elementos repetidos.
minhocasSemRepetidos :: Eq a => [a] -> Bool
minhocasSemRepetidos []     = True
minhocasSemRepetidos (x:xs) = not (x`elem` xs) && minhocasSemRepetidos xs

-- | Verifica se nenhuma 'Minhoca' está na mesma 'Posicao' que um 'Barril'.
minhocasForaDeBarris :: [Minhoca] -> [Objeto] -> Bool
minhocasForaDeBarris ms os =
  let psM = posicoesMinhocas ms
      psB = posicoesBarris os
  in null [p | p <- psM, p `elem` psB]

-- | Valida regras de 'VidaMinhoca' com base na 'Posicao'.
-- Se a 'Minhoca' não tem posição ('Nothing'), deve estar 'Morta'.
-- Se a 'Minhoca' está em 'Agua', deve estar 'Morta'.
-- Nos outros casos, a vida não é restringida por esta função (ver 'vidaOk').
regraAguaOuSemPosMorta :: Mapa -> Minhoca -> Bool
regraAguaOuSemPosMorta m w =
  case posicaoMinhoca w of
    Nothing -> vidaMinhoca w == Morta
    Just p  -> if eAgua (cel m p)
               then vidaMinhoca w == Morta
               else True

-- | Verifica se um valor 'VidaMinhoca' é válido.
-- 'Morta' é válido. 'Viva hp' é válido se 0 <= hp <= 100.
vidaOk :: VidaMinhoca -> Bool
vidaOk (Viva hp) = 0 <= hp && hp <= 100
vidaOk Morta     = True

-- | Verifica se todas as contagens de munições de uma 'Minhoca' são >= 0.
municoesOk :: Minhoca -> Bool
municoesOk w = all (>=0)
  [ jetpackMinhoca w
  , escavadoraMinhoca w
  , bazucaMinhoca w
  , minaMinhoca w
  , dinamiteMinhoca w
  ]

-- | Verifica se a lista de 'Minhoca's de um 'Estado' é válida.
-- Agrega todas as validações de minhocas:
validaMinhocas :: Estado -> Bool
validaMinhocas e =
  let m  = mapaEstado e
      os = objetosEstado e
      ms = minhocasEstado e
  in  all (minhocaPosicaoValida m) ms        
   && semSobreposicaoMinhocas ms             
   && minhocasForaDeBarris ms os            
   && all (regraAguaOuSemPosMorta m) ms      
   && all (vidaOk . vidaMinhoca) ms          
   && all municoesOk ms