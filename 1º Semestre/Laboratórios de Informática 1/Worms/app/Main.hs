module Main where

import Graphics.Gloss

import Desenhar
import Eventos
import Worms
import Tempo
import Labs2025
import Mapas



janela :: Display
janela = InWindow "Worms" (1920, 1080) (0, 0)


fundo :: Color
fundo = white


fr :: Int
fr = 2 


mapaAtual :: Int
mapaAtual = 6


estadoInicial :: Estado
estadoInicial =
  case mapaAtual of
    1 -> estadoMapa1
    2 -> estadoMapa2
    3 -> estadoMapa3
    _ -> estadoMapa1



-- | Função principal que invoca o Gloss para correr o jogo.
main :: IO ()
main = do
  putStrLn "Hello from Worms!"

  barril <- loadBMP "assets/barril.bmp"
  minhoca <- loadBMP "assets/minhoca.bmp"
  disparo <- loadBMP "assets/disparo.bmp"
  fundoMenu <- loadBMP "assets/angry_worms.bmp"
  imgTerra <- loadBMP "assets/terra.bmp"
  imgAgua <- loadBMP "assets/agua.bmp"
  imgPedra <- loadBMP "assets/pedra.bmp"
  mina <- loadBMP "assets/mina.bmp"
  dina <- loadBMP "assets/dinamite.bmp"
  vencedor1 <- loadBMP "assets/vencedor1.bmp"
  vencedor2 <- loadBMP "assets/vencedor2.bmp"

  let recursos = Recursos 
       { imgBarril = barril
      , imgMinhoca = minhoca
      , imgDisparo = disparo
      , imgFundoMenu = fundoMenu
      , imgTerra = imgTerra 
      , imgAgua  = imgAgua
      , imgPedra  = imgPedra
      , imgMina = mina
      , imgDinamite = dina 
      , imgVencedor1 = vencedor1
      , imgVencedor2 = vencedor2
      }
      it = Worms estadoInicial recursos (EmMenu Jogar) Este (0,0) Bazuca 
  play janela fundo fr it desenha reageEventos reageTempo

