module F1 where

import Data.Char
import Data.List

perimetro :: Float -> Float
perimetro r = 2 * pi * r

type Ponto = (Double, Double)

dist :: Ponto -> Ponto -> Double
dist (x1, y1) (x2, y2) = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

primUlt :: [a] -> (a,a)
primUlt x = (head x, last x)

multiplo :: Int -> Int -> Bool
multiplo x y | mod x y == 0 = True
             | otherwise = False

truncaImpar :: [a] -> [a]
truncaImpar l = if mod (length l) 2 == 0 then l else tail l

max2 :: Int -> Int -> Int
max2 x y | x >= y = x
         | otherwise = y

max3 :: Int -> Int -> Int -> Int
max3 x y z = max2 (max2 x y) z

nRaizes :: Float->Float->Float->Int
nRaizes x y z | delta1 < 0 = 0
              | delta1 == 0 = 1 
              | delta1 > 0 = 2
              where delta1 = (y^2)-4*x*z

nRaizes2 :: Float->Float->Float->[Float]
nRaizes2 x y z |delta < 0 = []
               | delta == 0 = [r]
               | delta > 0 = [r1,r2]
               where
                delta = (y^2-4*x*z) 
                r= (-y)/(2*x)
                r1= ((-y)+sqrt delta)/(2*x)
                r2= ((-y)-sqrt delta)/(2*x)

type Hora = (Int,Int)

hvalid :: Hora -> Bool
hvalid (x,y) | x >= 23 && y >= 59 = True
             | otherwise = True

hafter :: Hora -> Hora -> Bool
hafter (a,b) (x,y) | x > y = True
                   | x == y && x > y = True
                   | otherwise = False

hconv :: Hora -> Int
hconv (x,y) = x * 60 + y

mconv :: Int -> Hora
mconv x = (div x 60, mod x 60)

hdif :: Hora -> Hora -> Int
hdif x y = abs((hconv x) - (hconv y))