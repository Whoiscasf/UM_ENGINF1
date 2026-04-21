{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

{-|
Module      : Worms
Description : Tipos centrais e estado global do jogo Angry Worms.
Stability   : experimental

Este módulo define os tipos de dados fundamentais do jogo,
servindo como ponto central de ligação entre:

* Lógica temporal ('Tempo')
* Tratamento de eventos ('Eventos')
* Desenho gráfico ('Desenhar')

Não contém lógica de evolução do jogo nem desenho, apenas estruturas
de dados e funções auxiliares simples.
-}
module Worms where

import Labs2025
import Graphics.Gloss (Picture)


-- | Recursos gráficos utilizados pelo jogo.
--
-- Cada campo corresponde a uma imagem ou sprite carregado no início
-- da execução e reutilizado ao longo do jogo.
data Recursos = Recursos
  { imgBarril    :: Picture
  , imgMinhoca   :: Picture
  , imgDisparo   :: Picture 
  , imgFundoMenu :: Picture
  , imgTerra     :: Picture 
  , imgAgua      :: Picture
  , imgPedra     :: Picture
  , imgMina      :: Picture
  , imgDinamite  :: Picture
  , imgVencedor1 :: Picture
  , imgVencedor2 :: Picture
  }

-- | Estado global do jogo no Gloss.
--
-- Agrega toda a informação necessária para a execução do jogo:
--
-- * 'estado'    – estado lógico (mapa, objetos e minhocas).
-- * 'recursos'  – imagens e sprites carregados.
-- * 'modo'      – modo atual do jogo.
-- * 'direcoes'  – direção da minhoca ativa.
-- * 'rato'      – posição atual do rato (para a mira).
-- * 'armaAtual' – arma atualmente selecionada.
data Worms = Worms
  { estado    :: Estado
  , recursos  :: Recursos
  , modo      :: Modo    
  , direcoes  :: Direcao
  , rato      :: (Float, Float) 
  , armaAtual :: TipoArma
  }

-- | Obtém o terreno numa posição do mapa.
--
-- Convenção: @(l,c) = (linha, coluna)@.
-- Devolve 'Nothing' se a posição estiver fora dos limites do mapa.
lookupMapa :: Mapa -> (Int, Int) -> Maybe Terreno
lookupMapa mapa (l, c) =
  if l < 0 || l >= length mapa then Nothing
  else
    let linha = mapa !! l
    in if c < 0 || c >= length linha then Nothing
       else Just (linha !! c)

-- | Resultado final do jogo.
--
-- Contém o índice da minhoca vencedora.
data Resultado = Vencedor Int 
  deriving (Eq)

-- | Modos possíveis do jogo.
--
-- * 'EmMenu'  – menu principal
-- * 'EmJogo'  – jogo em execução
-- * 'EmAjuda' – ecrã de ajuda
-- * 'FimJogo' – ecrã final com vencedor
data Modo
  = EmMenu MenuOpcao
  | EmJogo
  | EmAjuda
  | FimJogo Resultado
  deriving (Eq)

-- | Opções disponíveis no menu principal.
data MenuOpcao = Jogar | Ajuda | Sair
  deriving (Eq, Enum, Bounded)

-- | Avança para a próxima opção do menu.
--
-- O ciclo é circular (de 'maxBound' volta a 'minBound').
proxima :: MenuOpcao -> MenuOpcao
proxima o | o == maxBound = minBound
          | otherwise     = succ o

-- | Regressa à opção anterior do menu.
--
-- O ciclo é circular (de 'minBound' volta a 'maxBound').
anterior :: MenuOpcao -> MenuOpcao
anterior o | o == minBound = maxBound
           | otherwise     = pred o
