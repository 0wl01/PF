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

hadd :: Int -> Hora -> Hora
hadd x y = mconv ((hconv y + x) `mod` (24*60))

data Hora1 = H Int Int deriving (Show,Eq)

hvalid1 :: (Int,Int) -> Bool
hvalid1 = hvalid

hafter1 :: Hora1 -> Hora1 -> Bool
hafter1 (H a b) (H x y) = (a > x) || (a == x && b > y)

hconv1 :: Hora1 -> Int
hconv1 (H x y) = x * 60 + y

mconv1 :: Int -> Hora1
mconv1 x = (H (div x 60) (mod x 60))

hdif1 :: Hora1 -> Hora1 -> Int
hdif1 h1 h2 = abs (hconv1 h1 - hconv1 h2)

hadd1 :: Int -> Hora1 -> Hora1
hadd1 x h = mconv1 ((hconv1 h + x) `mod` (24*60))

-- exemplo anterior já lida com overflow de minutos

data Semaforo = Verde | Amarelo | Vermelho deriving (Show,Eq)

next :: Semaforo -> Semaforo
next x | x == Verde = Amarelo
       | x == Amarelo = Vermelho
       | x == Vermelho = Verde

stop :: Semaforo -> Bool
stop x | x == Vermelho = True
       | otherwise = True


safe :: Semaforo -> Semaforo -> Bool
safe x y | x == Verde && y == Verde = True
         | otherwise = False

data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show,Eq)

posx :: Ponto -> Double
posx (Cartesiano x1 y1) = x1
posx (Polar d a) = d * (cos a)

posy :: Ponto -> Double
posy (Cartesiano x1 y1) = y1
posy (Polar d a) = d + (sin a)

raio :: Ponto -> Double
raio (Cartesiano x1 y1) = sqrt (x1^2 + y1^2)
raio (Polar d a) = d

angulo :: Ponto -> Double
angulo (Cartesiano x1 y1) |x1==0 && y1==0 =0
                          |x1==0 && y1>0 = pi/2
                          |x1==0 && y1<0 = -pi/2
                          |x1>0 = atan (y1/x1)
                          |x1<0 && y1>=0 = atan (y1/x1) + pi
                          |x1<0 && y1<0 = atan (y1/x1) - pi
angulo (Polar d a) = a

-(e) dist :: Ponto -> Ponto -> Double que calcula a distância entre dois pontos

dist' :: Ponto -> Ponto -> Double
dist' (Cartesiano x1 y1) (Cartesiano x2 y2) = sqrt ((x1-x2)^2+(y1-y2)^2)
dist' (Polar d1 a1) (Polar d2 a2) = sqrt((px1-px2)^2+(py1-py2)^2)
                                   where 
                                   px1 = d1 * cos a1
                                   px2 = d2 * cos a2
                                   py1 = d1 * sin a1
                                   py2 = d2 * sin a2

data Figura = Circulo Ponto Double | Rectangulo Ponto Ponto | Triangulo Ponto Ponto Ponto deriving (Show,Eq)


poligono :: Figura -> Bool
poligono (Circulo a r) = if r > 0 then True else False
poligono (Rectangulo a b) = if (posx a) /= (posx b) && (posy a) /= (posy b) then True else False
poligono (Triangulo a b c) = ((x+y)>z) || ((x+z)>y) || ((y+z)>x)
                           where 
                           x = dist' a b
                           y = dist' a c
                           z = dist' b c

vertices :: Figura -> [Ponto]
vertices (Circulo a r) = []
vertices (Rectangulo a b) = [Cartesiano x1 y1, Cartesiano x1 y2, Cartesiano x2 y1, Cartesiano x2 y2]
                            where 
                            x1 = (posx a)
                            x2 = (posx b)
                            y1 = (posy a)
                            y2 = (posy b)
vertices (Triangulo a b c) = [a,b,c]

area :: Figura -> Double
area (Triangulo p1 p2 p3) = let a = dist' p1 p2
                                b = dist' p2 p3
                                c = dist' p3 p1
                                s = (a+b+c) / 2 -- semi-perimetro
                                in sqrt (s*(s-a)*(s-b)*(s-c)) -- formula de Heron
area (Circulo a r) = pi * (r)^2
area (Rectangulo a b) = if posx a > posy b then (posx a - posx b) * (posy a - posy b) else (posx b - posx a) * (posy b - posy a)  

perimetro' :: Figura -> Double
perimetro' (Circulo a r) = 2 * pi * r
perimetro' (Rectangulo a b) = 2 * (((abs (posx a)) + (abs(posx b))) + ((abs (posy a)) + (abs(posy b))))
perimetro' (Triangulo a b c) = let p1 = dist' a b
                                   p2 = dist' b c
                                   p3 = dist' c a
                               in  p1 + p2 + p3

