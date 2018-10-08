module Task1_2 where

import Todo(todo)
import Prelude hiding (gcd, sin, cos)

factorial :: Double -> Double
factorial 0 = 1
factorial n = n * factorial (n - 1)

limitRadians :: Double -> Double
limitRadians x | ((x>=(-2*pi)) && (x<=2*pi)) = x
               | (x<(-2*pi)) = limitRadians (x+2*pi)
               | otherwise = limitRadians (x-2*pi)

-- синус числа (формула Тейлора)
sin :: Double -> Double
sin'' x = [((-1) ** n)*(x ** (2*n + 1)) / (factorial (2*n + 1)) | n <- [0..]]
-- берем 20 первых членов ряда (мне кажется этой точности достаточно)
sin x = sum (take 20 (sin'' (limitRadians x)))

-- косинус числа (формула Тейлора)
cos :: Double -> Double
cos'' x = [((-1) ** n)*(x ** (2*n)) / (factorial (2*n)) | n <- [0..]]
cos x = sum (take 20 (cos'' (limitRadians x)))

-- наибольший общий делитель двух чисел
gcd :: Integer -> Integer -> Integer
gcd x 0 = abs x
gcd 0 y = abs y
gcd x y = gcd b (mod a b)
   where a = abs x
         b = abs y

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
pow :: Integer -> Integer -> Integer
pow x 0 = 1
pow x 1 = x
pow x y | even y = pow (x*x) (y `div` 2)
        | otherwise = x * (pow (x*x) ((y-1) `div` 2))

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime x | (length [n | n <- [2 .. x-1], mod x n == 0]) > 0 = False
          | otherwise = True