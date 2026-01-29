module F4 where

import Data.Char
import Data.List

-- Exercicio 1

{-
Defina a fun¸c˜ao digitAlpha :: String -> (String,String), que dada uma string,
devolve um par de strings: uma apenas com as letras presentes nessa string, e a outra
apenas com os n´umeros presentes na string. Implemente a fun¸c˜ao de modo a fazer uma
´unica travessia da string. Relembre que as fun¸c˜oes isDigit,isAlpha :: Char -> Bool
est˜ao j´a definidas no m´odulo Data.Char.
-}

digitAlpha :: String -> (String,String)
digitAlpha [] = ([] , [])
digitAlpha (h:t) | isDigit h = (a , h:b)
                 | isAlpha h = (h:a , b)
                 | otherwise = (a,b)
                 where
                    (a,b) = digitAlpha t


--Exercicio 2
{-
Defina a fun¸c˜ao nzp :: [Int] -> (Int,Int,Int) que, dada uma lista de inteiros,
conta o n´umero de valores nagativos, o n´umero de zeros e o n´umero de valores positivos,
devolvendo um triplo com essa informa¸c˜ao. Certifique-se que a fun¸c˜ao que definiu
percorre a lista apenas uma vez.
-}

nzp :: [Int] -> (Int,Int,Int)
nzp [] = (0,0,0)
nzp (h:t) | h < 0 = (a+1,b,c)
          | h == 0 = (a,b+1,c)
          | h > 0 = (a,b,c+1)
          where
            (a,b,c) = nzp t

--Exercicio 3
{-
Defina a fun¸c˜ao divMod' :: Integral a => a -> a -> (a, a) que calcula simultane-
amente a divis˜ao e o resto da divis˜ao inteira por subtrac¸c˜oes sucessivas.
-}

divMod' :: Integral a => a -> a -> (a, a)
divMod' x y | x < y = (0,x)
           | x >= y = (divI + 1, res)
           where
            (divI,res) = divMod' (x-y) y

--Exercicio 4
{-
Utilizando uma fun¸c˜ao auxiliar com um acumulador, optimize seguinte defini¸c˜ao recur-
siva que determina qual o n´umero que corresponde a uma lista de digitos.
fromDigits :: [Int] -> Int
fromDigits [] = 0
fromDigits (h:t) = h*10^(length t) + fromDigits t
Note que
fromDigits [1,2,3,4] = 1 × 103 + 2 × 102 + 3 × 101 + 4 × 100
= 4 + 10 × (3 + 10 × (2 + 10 × (1 + 10 × 0)))
-}~

fromDigits' :: [Int] -> Int
fromDigits' l = aux 0 l
    where
        aux :: Int -> [Int] -> Int
        aux n [] = n
        aux n (x:xs) = aux (n*10+x) xs

--Exercicio 5
{-
Utilizando uma fun¸c˜ao auxiliar com acumuladores, optimize seguinte defini¸c˜ao que
determina a soma do segmento inicial de uma lista com soma m´axima.
maxSumInit :: (Num a, Ord a) => [a] -> a
maxSumInit l = maximum [sum m | m <- inits l]
-}

maxSumInit :: (Num a, Ord a) => [a] -> a
maxSumInit [] = 0
maxSumInit l = aux 0 0 l
    where
        aux somaAtual maximo [] = maximo
        aux somaAtual maximo (x:xs) = aux novaSoma novoMaximo xs
            where
                novaSoma = somaAtual + x
                novoMaximo = if novaSoma > maximo then novaSoma else maximo

--Exercicio 6
{-
Optimize a seguinte defini¸c˜ao recursiva da fun¸c˜ao que calcula o n-´esimo n´umero da
sequˆencia de Fibonacci, usando uma fun¸c˜ao auxiliar com 2 acumuladores que represen-
tam, respectivamente, o n-´esimo e o n+1-´esimo n´umeros dessa sequˆencia.
-}

fib :: Int -> Int
fib n = aux (0,1) n
    where
        aux :: (Int,Int) -> Int -> Int
        aux (a,b) 0 = a
        aux (a,b) 1 = b
        aux (a,b) x | x>1 = aux (b,a+b) (x-1)

--Exercicio 7
{-
Defina a fun¸c˜ao intToStr :: Integer -> String que converte um inteiro numa
string. Utilize uma fun¸c˜ao auxiliar com um acumulador onde vai construindo a string
que vai devolver no final.
1
-}

intToStr :: Integer -> String
intToStr n | n == 0 = '0'
           | otherwise = aux n ""
            where
                aux :: Integer -> String -> String
                aux 0 acc = acc
                aux valor acc = aux resto (caracter : acc)
                    where digito = valor `mod` 10
                          resto = valor `div` 10
                          caracter = chr (fromIntegral digito + ord '0')

--Exercicio 8
{-
Para cada uma das express˜oes seguintes, exprima por enumera¸c˜ao a lista correspon-
dente. Tente ainda, para cada caso, descobrir uma outra forma de obter o mesmo
resultado.
-}

--(a) [x | x <- [1..20], mod x 2 == 0, mod x 3 == 0]
--Divisivel por 2 e por 3 logo resulta nos multiplos de 6

--(b) [x | x <- [y | y <- [1..20], mod y 2 == 0], mod x 3 == 0]
--[y | y <- [1..20], mod y 2 == 0] vai resultar na lista com os multiplos de 2 até a 20
--O resultado final depois é os multiplos de 2 divisiveis por 3. o mesmo que A os multiplos de 6

--(c) [(x,y) | x <- [0..20], y <- [0..20], x+y == 30]
-- Uma lista de tuplos em que a soma seja 30 ex: [(15,15)]

--(d) [sum [y | y <- [1..x], odd y] | x <- [1..10]]
--[1, 1, 4, 4, 9, 9, 16, 16, 25, 25]