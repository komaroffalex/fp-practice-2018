module Task2_2 where

import Todo(todo)

import Prelude hiding (foldl, foldr, unfoldr, map, concatMap, 
    filter, maxBy, minBy, reverse, sum, product, elem)

import Data.List hiding (foldl, foldr, unfoldr, map, concatMap, 
    filter, maxBy, minBy, reverse, sum, product, elem)

import Data.Maybe hiding (foldl, foldr, unfoldr, map, concatMap, 
    filter, maxBy, minBy, reverse, sum, product, elem, catMaybes)

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
sum x = foldl (+) 0 x

-- Переворот списка (Пример)
reverse :: [a] -> [a]
reverse [] = []  
reverse x = foldl (\xs x'-> x' : xs) [] x 

-- Отображение элементов списка
map :: (a -> b) -> [a] -> [b]
map f x = foldr (\x acc -> f x : acc) [] x

-- Произведение всех элементов списка
product :: [Integer] -> Integer
product []     = 1
product xs = foldl (*) 1 xs

-- Выделение из списка Maybe всех существующих значений
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes x = map fromJust (filterNot isNothing x)

-- Диагональ матрицы
diagonal :: [[a]] -> [a]
diagonal mat = foldr (\str acc -> (str !! ((length str - 1) - (length acc)) : acc)) [] mat

-- Фильтр для всех элементов, не соответствующих предикату
filterNot :: (a -> Bool) -> [a] -> [a]
filterNot _ [] = []
filterNot p xs = foldr (\x xs -> if p x then xs else x : xs) [] xs

-- Поиск элемента в списке
elem :: (Eq a) => a -> [a] -> Bool
elem el [] = False
elem x list = (filterNot ( /= x) list) /= []

-- Список чисел в диапазоне [from, to) с шагом step
rangeTo :: Integer -> Integer -> Integer -> [Integer]
rangeTo from to step = unfoldr (\n -> if n < to then Just (n, n + step) else Nothing) from

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
groups l n = unfoldr (\xs -> if null xs then Nothing else (Just (splitAt k xs))) l 
               where k = fromIntegral n

testMat = [[1,2,3],[4,5,6],[7,8,9]]