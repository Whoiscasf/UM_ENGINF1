data Hora = H Int Int
            deriving Show

type Etapa = (Hora,Hora)
type Viagem = [Etapa]

viagem = [(H 9 30, H 10 25), (H 11 20,H 12 45), (H 13 30, H 14 45)]

horavalida :: Hora -> Bool
horavalida (H h m) = h>=0 && h<24 && m>=0 && m<60

horamaior :: Hora -> Hora -> Bool -- vai dar True se h1>h2
horamaior (H h1 m1) (H h2 m2) = if h1>h2 then True 
                                else if h2>h1 then False
                                     else m1 > m2

etapaValida :: Etapa -> Bool
etapaValida (hi,hf) = horavalida hi &&
                 horavalida hf &&
                 horamaior hf hi

viagemValida :: Viagem -> Bool
viagemValida [e] = etapaValida e
viagemValida (e1:e2:t)
    | etapaValida e1 && horamaior (fst e2) (snd e1) = viagemValida (e2:t)
    | otherwise = False

partidaChegada :: Viagem -> (Hora,Hora)
partidaChegada [e] = e 
partidaChegada (e:t) = (fst e,snd(last t))

tempoViagem :: Viagem -> Hora
tempoViagem (a:t) = minutosparaHora (horaparaMinutos h2 - horaparaMinutos h1)
    where
    h1 = fst a
    h2 = snd (last (t))

horaparaMinutos:: Hora -> Int
horaparaMinutos (H h m) = h*60 + m
minutosparaHora :: Int -> Hora
minutosparaHora m = H (m `div` 60) (m `mod` 60)

somaHoras :: Hora -> Hora -> Hora
somaHoras h1 h2 = minutosparaHora (horaparaMinutos h1 + horaparaMinutos h2)

tempoEspera :: Viagem -> Hora
tempoEspera [_] = H 0 0
tempoEspera ((h1,h2):(h3,h4):t) = somaHoras (minutosparaHora (horaparaMinutos h3 - horaparaMinutos h2)) (tempoEspera ((h3,h4):t))

tempoTotal :: Viagem ->Hora
tempoTotal []= H 0 0
tempoTotal [e] = tempoViagem [e]
tempoTotal ((h1,h2):(h3,h4):t) = somaHoras (tempoEspera ((h1,h2):(h3,h4):t))(tempoViagem ((h1,h2):(h3,h4):t))

-- Exercício 2
type Ponto = (Double,Double)

data Figura = Circulo Ponto Double
        | Rectangulo Ponto Ponto
        | Triangulo Ponto Ponto Ponto
    deriving (Show,Eq)

type Poligonal = [Ponto]
dist :: (Double, Double) -> (Double, Double) -> Double
dist (x1,y1) (x2, y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)


area :: Figura -> Double
area (Triangulo p1 p2 p3) =
    let a = dist p1 p2
        b = dist p2 p3
        c = dist p3 p1
        s = (a+b+c) / 2 -- semi-perimetro
    in sqrt (s*(s-a)*(s-b)*(s-c)) -- formula de Heron

triangula :: Poligonal -> [Figura]
triangula (p1:p2:p3:p4:lp) = (Triangulo p1 p2 p3) : triangula (p1:p3:lp)
triangula _ = []

arealp :: Poligonal -> Double
arealp lp = let listaTriangulos = triangula lp
            in somaAreas listaTriangulos
somaAreas :: [Figura] -> Double
somaAreas [] = 0
somaAreas (t:ts) = area t + somaAreas ts