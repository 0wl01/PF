module F3 where

import Data.Char
import Data.List
import F1

-- Exercicio 1
{-
Assumindo que uma hora ´e representada por um par de inteiros, uma viagem pode ser
representada por uma sequˆencia de etapas, onde cada etapa ´e representada por um par
de horas (partida, chegada):
-}

data Hora = H Int Int
    deriving Show

type Etapa = (Hora,Hora)
type Viagem = [Etapa]

{-
(a) Testar se uma etapa est´a bem constru´ıda (i.e., o tempo de chegada ´e superior ao
de partida e as horas s˜ao v´alidas).
-}

etapaA :: [Etapa] -> Bool
etapaA (h1,h2) = hDepois' h1 h2

{- 
(b) Testa se uma viagem está bem construída (i.e., se para cada etapa, o tempo de chegada é superior ao de partida, e se a etapa 
seguinte começa depois da etapa anterior ter terminado). 
-}

valiViagem :: Viagem -> Bool
valiViagem [] = True
valiViagem [x] = etapaA x
valiViagem (e1:e2:t) = etapaA e1 && etapaA (snd e1 , fst e2) && valiViagem (e2:t)

{-
(c) Calcular a hora de partida e de chegada de uma dada viagem.
-}

horas :: [Viagem] -> Etapa
horas p = (fst (head p) , snd (last p))

{-
(d) Dada uma viagem v´alida, calcular o tempo total de viagem efectiva
-}

vValida :: Viagem -> Int
vValida [] = 0
vValida ((h1,h2) :t) = difM' h2 h1 + vValida t

--(e) Calcular o tempo total de espera.

tempoEspera :: Viagem -> Int
tempoEspera ((a,b):(c,d):t) = difM' b a + tempoEspera ((c,d):t)
tempoEspera _ = 0

--(f) Calcular o tempo total da viagem (a soma dos tempos de espera e de viagem efectiva).

tempoTotal :: Viagem -> Int
tempoTotal x = vValida x + tempoEspera x


--Não achei a pena repetir este exercicio

-- 2. Considere as seguinte definição de um tipo para representar linhas poligonais.

type Poligonal = [Ponto]
-- Relembrar: data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show,Eq)

{-- O tipo Ponto é idêntico ao definido na Ficha 1. Nas resolução das alíneas seguintes
pode utilizar funções definidas nessa ficha. -}

--(a) Defina a função para calcular o comprimento de uma linha poligonal.

lenPoligonal :: Poligonal -> Double
lenPoligonal (x:y:xs) = dist' x y + lenPoligonal (y:xs) 
lenPoligonal _ = 0

--(b) Defina uma função para testar se uma dada linha poligonal é ou não fechada.

closePoligonal :: Poligonal -> Bool
closePoligonal l = if (dist' (head l) (last l)) == 0 then True else False

{-(c) Defina a função triangula :: Poligonal -> [Figura] que, dada uma linha
poligonal fechada e convexa, calcule uma lista de triângulos cuja soma das áreas
seja igual à área delimitada pela linha poligonal. O tipo Figura é idêntico ao
definido na Ficha 1. -}

formaTri :: Poligonal -> [Figura]
formaTri (x:y:z:xs) = (Triangulo x y z) : formaTri (x:z:xs)
formaTri _ = []


--(d) Defina uma função para calcular a área delimitada por uma linha poligonal fechada e convexa.

areaPoligonal :: Poligonal -> Double
areaPoligonal l = aux (formaTri l)
                  where aux::[Figura]->Double
                        aux [] = 0
                        aux (x:xs) = area  x + aux xs

{- (e) Defina a função mover :: Poligonal -> Ponto -> Poligonal que, dada uma
linha poligonal e um ponto, dá como resultado uma linha poligonal idêntica à
primeira mas tendo como ponto inicial o ponto dado. -}

mover :: Poligonal -> Ponto -> Poligonal
mover pol p = p:pol

{- (f) Defina a função zoom :: Double -> Poligonal -> Poligonal que, dada um
factor de escala e uma linha poligonal, dê como resultado uma linha poligonal
semelhante e com o mesmo ponto inicial mas em que o comprimento de cada
segmento de recta é multiplicado pelo factor dado. -}

zoom :: Double -> Poligonal -> Poligonal
zoom z [p1@(Cartesiano x y),p2@(Cartesiano a b)] = p1:(Cartesiano (z*a) (z*b)):[]
zoom z (p1@(Cartesiano x y):p2@(Cartesiano a b):pol) = p1:zoom z (p3:pol)
    where p3 = (Cartesiano (z*a) (z*b))

-- Exercicio 3

data Contacto = Casa Integer
              | Trab Integer
              | Tlm Integer
              | Email String
                deriving Show

type Nome = String
type Agenda = [(Nome,[Contacto])]

{-
(a) Defina a fun¸c˜ao acrescEmail :: Nome -> String -> Agenda -> Agenda que,
dado um nome, um email e uma agenda, acrescenta essa informa¸c˜ao `a agenda.
-}

acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail nome email agenda = agenda ++ [(nome,[Email email])]

{-
Defina a fun¸c˜ao verEmails :: Nome -> Agenda -> Maybe [String] que, dado
um nome e uma agenda, retorna a lista dos emails associados a esse nome. Se esse
nome n˜ao existir na agenda a fun¸c˜ao deve retornar Nothing
-}

verEmails :: Nome -> Agenda -> Maybe [String]
verEmails nome agenda = case agenda of
    [] -> Nothing
    ((n, contactos) : t) ->
        if nome == n then
            Just (encontraEmail contactos)
            else
                verEmails nome t
    where
        encontraEmail :: [Contacto] ->  [String]
        encontraEmail [] = []
        encontraEmail (Email e : t) = e : encontraEmail t
        encontraEmail (_:t) = encontraEmail t

{-
(c) Defina a fun¸c˜ao consTelefs :: [Contacto] -> [Integer] que, dada uma lista
de contactos, retorna a lista de todos os n´umeros de telefone dessa lista (tanto
telefones fixos como telem´oveis).
-}

consTelefs :: [Contacto] -> [Integer]
consTelefs p = case p of
    [] -> []
    (Casa n : t) -> n : consTelefs t
    (Trab n : t) -> n : consTelefs t
    (Tlm n : t) -> n : consTelefs t
    (_ : t) -> consTelefs t

{-
(d) Defina a fun¸c˜ao casa :: Nome -> Agenda -> Maybe Integer que, dado um nome
e uma agenda, retorna o n´umero de telefone de casa (caso exista).
-}

casa :: Nome -> Agenda -> Maybe Integer
casa nome agenda = case agenda of
    [] -> Nothing
    ((n , contactos) : t) ->
        if nome == n then
            (encontraCasa contactos)
            else 
                casa nome t
    where
        encontraCasa :: [Contacto] -> Maybe Integer
        encontraCasa [] = Nothing
        encontraCasa (Casa x : t) = Just x
        encontraCasa (_ : t) = encontraCasa t