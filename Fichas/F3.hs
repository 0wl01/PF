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

--Exercicio 4

type Dia = Int
type Mes = Int
type Ano = Int
type Nome = String
data Data' = D' Dia Mes Ano deriving Show
type TabDN = [(Nome,Data')]

{-
Defina a funçao procura :: Nome -> TabDN -> Maybe Data, que indica a data
de nascimento de uma dada pessoa, caso o seu nome exista na tabela.
-}

procura :: Nome -> TabDN -> Maybe Data'
procura nome ((nom,dat):tabDn) = if nome == nom then Just dat else procura nome tabDn

--(b) Defina a função idade :: Data' -> Nome -> TabDN -> Maybe Int, que calcula a idade de uma pessoa numa dada data.

idade :: Data' -> Nome -> TabDN -> Maybe Int
idade (D' d1 m1 a1) nome ((nom,(D' d2 m2 a2 )):t) | nome /= nom = idade (D' d1 m1 a1) nome t
                                                  | a1 < a2 || (a1 == a2 && m1 < m2) = Just 0
                                                  | m1 < m2 || (m1 == m2 && d1 < d2) = Just (a1-a2-1)
                                                  | otherwise = Just (a1-a2)

--(c) Defina a função anterior :: Data' -> Data' -> Bool, que testa se uma data é anterior a outra data.

anterior :: Data' -> Data' -> Bool
anterior (D' d1 m1 a1) (D' d2 m2 a2) = if  (a1<a2) || (a1 == a2 && m1 < m2) || (a1 == a2 && m1 == m2 && d1 < d2) then True else False

--(d) Defina a função ordena :: TabDN -> TabDN, que ordena uma tabela de datas de nascimento, por ordem crescente das datas de nascimento.

ordena' :: TabDN -> TabDN
ordena' [] = []
ordena' ((nom,dat):t) = aux (nom,dat)  (ordena' t)
                       where aux (no,da) [] = [(no,da)]
                             aux (no,da) ((n,d):t) = if (anterior da d) == False then (n,d) : aux (no,da) t else (no,da) : aux (n,d) t       

--(e) Defina a função porIdade:: Data' -> TabDN -> [(Nome,Int)], que apresenta o nome e a idade das pessoas, numa dada data, por ordem crescente da idade das pessoas.

porIdade:: Data' -> TabDN -> [(Nome,Int)]
porIdade _ [] = []
porIdade (D' d1 m1 a1) tabDn = (nom,idade) : porIdade (D' d1 m1 a1) tabDn
                                where ((nom,(D' d' m' a')):t) = ordena' tabDn
                                      idade = if (m1 < m' || (m1 == m' && d1 < d')) then (a1-a'-1) else (a1-a')

{- 5. Considere o seguinte tipo de dados que descreve a informação de um extracto bancário.
Cada valor deste tipo indica o saldo inicial e uma lista de movimentos. Cada movimento
é representado por um triplo que indica a data da operação, a sua descrição e a quantia
movimentada (em que os valores são sempre números positivos). -}

data Movimento = Credito Float | Debito Float deriving Show
data Data = D Int Int Int deriving Show
data Extracto = Ext Float [(Data, String, Movimento)] deriving Show

--(a) Construa a função extValor :: Extracto -> Float -> [Movimento] que produz uma lista de todos os movimentos (créditos ou débitos) superiores a um determinado valor.

extValor :: Extracto -> Float -> [Movimento]
extValor (Ext si []) val = []
extValor (Ext si ((_,_,Credito x):t)) val = if x > val then (Credito x) : extValor (Ext si t) val else extValor (Ext si t) val 
extValor (Ext si ((_,_,Debito x):t)) val = if x > val then (Debito x) : extValor (Ext si t) val else extValor (Ext si t) val 

{-(b) Defina a função filtro :: Extracto -> [String] -> [(Data,Movimento)] que retorna informação relativa apenas aos movimentos cuja descrição esteja incluída
na lista fornecida no segundo parâmetro. -}

filtro :: Extracto -> [String] -> [(Data,Movimento)]
filtro (Ext si []) _ = []
filtro (Ext si ((d,s,mov):t)) st = if elem s st then (d,mov) : filtro (Ext si t) st else  filtro (Ext si t) st

{- (c) Defina a função creDeb :: Extracto -> (Float,Float), que retorna o total de créditos e de débitos de um extracto no primeiro e segundo elementos de um par,
respectivamente. -}

creDeb :: Extracto -> (Float,Float)
creDeb (Ext si resto) = (cred resto, deb resto)
                                where cred :: [(Data, String, Movimento)] -> Float
                                      cred [] = 0
                                      cred ((d,s,Credito x):t) = x + cred t
                                      cred ((d,s,Debito x):t) = cred t
                                      deb :: [(Data, String, Movimento)] -> Float
                                      deb [] = 0
                                      deb ((d,s,Debito x):t) = x + deb t
                                      deb ((d,s,Credito x):t) = deb t

--(d) Defina a função saldo :: Extracto -> Float que devolve o saldo final que resulta da execução de todos os movimentos no extracto sobre o saldo inicial.

saldo :: Extracto -> Float
saldo (Ext si resto) = si + d - c
                       where (d,c) = creDeb (Ext