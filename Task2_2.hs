module Task2_2 where

import Todo(todo)

import Prelude hiding (foldl, foldr, unfoldr, map, concatMap, 
    filter, maxBy, minBy, reverse, sum, product, elem)

import Data.List hiding (foldl, foldr, unfoldr, map, concatMap, 
    filter, maxBy, minBy, reverse, sum, product, elem)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f a [] = a
foldl f a (h : t) = foldl f (f a h) t

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f a [] = a
foldr f a (h : t) = f h (foldr f a t)

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f b = case f b of
    Just (a, b') -> a : unfoldr f b'
    Nothing -> []

-- Сумма всех элементов списка (пример)
sum :: [Integer] -> Integer
sum []     = 0
sum (x:xs) = x + sum xs

-- Переворот списка (Пример)
reverse :: [a] -> [a]
reverse [] = []  
reverse (x:xs) = reverse xs ++ [x]  

-- Отображение элементов списка
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

-- Произведение всех элементов списка
product :: [Integer] -> Integer
product []     = 1
product xs = foldl (*) 1 xs

-- Выделение из списка Maybe всех существующих значений
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes xs = [ x | Just x <- xs ]

-- Диагональ матрицы
diagonal :: [[a]] -> [a]
diagonal mat = 
    let n = length (mat !! 0)
    in  [(mat !! y) !! x |  x <- [0..(n - 1)], 
                            y <- [0..(n - 1)],
                            y == x]

-- Фильтр для всех элементов, не соответствующих предикату
filterNot :: (a -> Bool) -> [a] -> [a]
filterNot _ [] = []
filterNot f (x:xs)
    | not(f x)  = x : (filterNot f xs)
    | otherwise = filterNot f xs

-- Поиск элемента в списке
elem :: (Eq a) => a -> [a] -> Bool
elem el [] = False
elem el (x:xs)
    | el == x = True
    | otherwise = elem el xs

-- Список чисел в диапазоне [from, to) с шагом step
rangeTo :: Integer -> Integer -> Integer -> [Integer]
rangeTo from to step = [from, from + step.. to]

-- Конкатенация двух списков
append :: [a] -> [a] -> [a]
append [] [] = []
append xs [] = xs
append [] ys = ys
append xs ys = foldr (:) ys xs

-- Разбиение списка lst на куски размером n
-- (последний кусок может быть меньше)
groups :: [a] -> Integer -> [[a]]
groups [] _ = []
groups l n
    | n > 0 = (genericTake n l) : (groups (genericDrop n l) n)
    | otherwise = error "Negative n"
