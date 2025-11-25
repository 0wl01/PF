import Data.Char
import Data.List
--Ficha 2
--Exercicio 1

--a)
funA :: [Double] -> Double
funA [] = 0
funA (y:ys) = y^2 + (funA ys)
{-
Diga, justificando, qual ´e o valor de funA [2,3,5,1]
Vai somar os quadrados de cada elemento da lista
Resposta: 4 + 9 + 25 + 1 = 39
-}

--b)
funB :: [Int] -> [Int]
funB [] = []
funB (h:t) = if (mod h 2)==0 
    then h : (funB t)
    else (funB t)
{-
Diga, justificando, qual ´e o valor de funB [8,5,12]
Vai retirar os valores impares da lista
Resposta: [8.12]
-}

--c)
funC (x:y:t) = funC t
funC [x] = [x]
funC [] = []
{-
Diga, justificando, qual ´e o valor de funC [1,2,3,4,5]
A função vai remover os dois primeiros elementos de uma lista.
Logo se for uma lista de comprimento impar devolve o ultimo elemento
Caso contrário devolve a lista vazia
Resposta: [5]
-}

--d)
funD l = g [] l
g acc [] = acc
g acc (h:t) = g (h:acc) t
{-
Diga, justificando, qual ´e o valor de funD "otrec"
A função usa um acumulador (acc) chamada em fundD como '[]'
A funcao g vai colocando recursivamente a cabeça da lista no inicio do acumulador
Efetivamente revertendo a String recebida
"otrec" vira "certo"
-}

--Exercicio 2

--a)
--Versão Recursiva
dobros :: [Float] -> [Float]
dobros [] = [] 
dobros (h:t) = h * 2 : dobros t
--Versão eficiente
dobros1 :: [Float] -> [Float]
dobros1 = map (*2)

--b)
--Versão Recursiva
numOcorre :: Char -> String -> Int
numOcorre _ [] = 0
numOcorre x (h:t) = if x == h 
                    then 1 + numOcorre x t
                    else numOcorre x t
--Versão Eficiente
numOcorre1 :: Char -> String -> Int
numOcorre1 x = foldl (\acc h -> if h == x then acc + 1 else acc) 0

--c)
--Versão Recursiva
positivos :: [Int] -> Bool
positivos [] = True
positivos (h:t) = if h > 0 
                  then positivos t 
                  else False
--Versão Eficiente
positivos1 :: [Int] -> Bool
positivos1 = all (>0)

--d)
--Versão Recursiva
soPos :: [Int] -> [Int]
soPos [] = []
soPos (h:t) = if h > 0
              then h : soPos t
              else soPos t
--Versão Eficiente
soPos1 :: [Int] -> [Int]
soPos1 = filter (>0)

--f)
--Versão Recursiva
tresUlt :: [a] -> [a]
tresUlt (a:b:c:t) = tresUlt (b:c:t)
tresUlt l = l
--Versão Eficiente
tresUlt1 :: [a] -> [a]
tresUlt1 x = drop (length x - 3) x

tresUlt2 :: [a] -> [a]
tresUlt2 = reverse . take 3 . reverse
--Esta é fixe, inverte a lista , rouba 3 e inverte outra vez

--g)
--Versão Recursiva
segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((a,b):t) = b : segundos t
--Versão Eficiente
segundos1 :: [(a,b)] -> [b]
segundos1 = map (snd)

--h)
--Versão Recursiva
nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros x [] = False
nosPrimeiros x ((a,b):t) = 
    if x == a 
        then True
        else nosPrimeiros x t
--Versão Eficiente
nosPrimeiros1 :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros1 x = any (\(a,_) -> x == a)

nosPrimeiros2 :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros2 x = any ((==x) . fst)

--i)
--Versão Recursiva
sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos ((a,b,c):t) = (a+x, b+y, c+z)
                        where (x,y,z) = sumTriplos t
--Versão Eficiente
sumTriplos1 :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos1 = foldl' (\(x,y,z) (a,b,c) -> (x+a, y+b, z+c)) (0,0,0)
--se usares foldl aqui pode dar stack overflow por exemplo

--Exercicio 3