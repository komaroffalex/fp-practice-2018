module Task2_1 where

import Todo(todo)
import Prelude hiding (lookup)

-- Ассоциативный массив на основе бинарного дерева поиска
-- Ключи - Integer, значения - произвольного типа
data TreeMap v =    EmptyTree 
                    | Leaf Integer v
                    | Node Integer v (TreeMap v) (TreeMap v) deriving(Show) 
                    
-- Пустое дерево
emptyTree :: TreeMap v
emptyTree = EmptyTree

-- Содержится ли заданный ключ в дереве?
contains :: TreeMap v -> Integer -> Bool
contains EmptyTree _ = False
contains (Leaf key v) k = key == k
contains (Node key v lt rt) k | k < key = contains lt k
                              | k > key = contains rt k
                              | otherwise = True

-- Значение для заданного ключа
lookup :: Integer -> TreeMap v -> v
lookup k EmptyTree = error "The tree is empty."
lookup k (Leaf key v)           | k == key = v
                                | otherwise = error "No such element in leaf."

lookup k (Node key v (Leaf key' v') (Leaf key'' v''))     
                                | k == key = v
                                | k == key' = v'
                                | k == key'' = v''
                                | otherwise = error "No such element."

lookup k (Node key v (Leaf key' v') EmptyTree)     
                                | k == key = v
                                | k == key' = v'                                
                                | otherwise = error "No such element."

lookup k (Node key v EmptyTree (Leaf key' v'))     
                                | k == key = v
                                | k == key' = v'                                
                                | otherwise = error "No such element."

lookup k (Node key v EmptyTree rt)     
                                | k == key = v
                                | otherwise = lookup k rt                                
                                
lookup k (Node key v lt EmptyTree)     
                                | k == key = v
                                | otherwise = lookup k lt                                   

lookup k (Node key v lt rt)     | k < key = lookup k lt
                                | k > key = lookup k rt
                                | otherwise = v

-- Вставка пары (ключ, значение) в дерево
infixl 8 `insert`
insert :: (Integer, v) -> TreeMap v -> TreeMap v
insert (k, v) EmptyTree = Leaf k v
insert (k, v) (Leaf key val)        
                        | k < key = Node key val (Leaf k v) (EmptyTree)
                        | k > key = Node key val (EmptyTree) (Leaf k v)
                        | otherwise = Leaf k v

insert (k, v) (Node key val lt rt)  
                        | k < key = (Node key val (insert (k,v) lt) rt)
                        | k > key = (Node key val lt (insert (k,v) rt))
                        | otherwise = Node k v lt rt

-- Удаление элемента по ключу
remove :: Integer -> TreeMap v -> TreeMap v
remove i EmptyTree = EmptyTree
remove i (Leaf key v)           
                        | i == key = EmptyTree
                        | otherwise = Leaf key v       
                                
remove i (Node key v EmptyTree (Leaf key' v'))      
                        | i == key = Leaf key' v'
                        | i == key' = Leaf key v
                        | otherwise = Node key v EmptyTree (Leaf key' v') 

remove i (Node key v (Leaf key' v') EmptyTree)      
                        | i == key = Leaf key' v'
                        | i == key' = Leaf key v
                        | otherwise = Node key v (Leaf key' v') EmptyTree

remove i (Node key v lt rt)     
    | i < key = (Node key v (remove i lt) rt)
    | i > key = (Node key v lt (remove i rt))
    | otherwise =   
        if isEmpty rt
        then lt
        else Node leftmostKey leftmostV lt rt'
            where
                isEmpty EmptyTree = True
                isEmpty _ = False
                (leftmostKey, leftmostV, rt') = deleteLeftmostEl rt
                deleteLeftmostEl (Leaf key value) = (key, value, EmptyTree)
                deleteLeftmostEl (Node key value EmptyTree rt) = (key, value, rt)
                deleteLeftmostEl (Node key value lt rt) =
                    (key', value', (Node key value lt' rt))
                    where (key', value', lt') = deleteLeftmostEl lt

-- Поиск ближайшего снизу ключа относительно заданного
nearestLE :: Integer -> TreeMap v -> (Integer, v)
nearestLE x EmptyTree = error "The tree is empty."

nearestLE x (Leaf key val)      
                    | key <= x = (key,val)
                    | otherwise = error "No such element in leaf1."

nearestLE x (Node key val EmptyTree rt)                 
                    | key < x = nearestLE x rt
                    | key == x = (key, val)
                    | otherwise = error "No such element in tree." 

nearestLE x (Node key val lt EmptyTree)                 
                    | key >= x = (key, val)
                    | otherwise = error "No such element in tree." 

nearestLE x (Node key val (Leaf key' val') (Leaf key'' val''))                      
        | x==key = (key,val)
        | x > key = if key''> x     then (key,val)
                                    else (key'',val'')
        | otherwise = if x >= key'  then (key', val')
                                    else error "No such element in leaf2." 

nearestLE x (Node key val (Leaf key' val') rt)                      
        | x == key = (key,val)
        | x < key = if x > key'     then (key', val')
                                    else error "No such element in tree."
        | otherwise = nearestLE x rt

nearestLE x (Node key val (Node key' val' lt' rt') rt)              
    | x == key = (key,val)
    | x < key = if x > key'     
                then    if isNode rt' 
                        then nearestLE x rt'
                        else    if isEmptyNode rt' 
                                then (key',val') 
                                else    if getLeafKey rt' > x 
                                        then (key', val') 
                                        else nearestLE x rt'
                else nearestLE x lt'
    | otherwise =   if getLeftmostEl rt <= x 
                    then nearestLE x rt
                    else (key, val)
        where
            isNode (Node key'' val'' lt'' rt'') = True
            isNode _ = False
            isEmptyNode EmptyTree = True
            isEmptyNode _ = False
            getLeafKey (Leaf key'' val'') = key''
            getLeafKey _ = undefined   
            getLeftmostEl (Leaf key value) = key
            getLeftmostEl (Node key value EmptyTree rt) = key 
            getLeftmostEl (Node key value lt rt) = ltKey 
                where ltKey = getLeftmostEl lt

infixl 9 `insert'`
insert' :: TreeMap v -> (Integer, v) -> TreeMap v
insert' EmptyTree (k, v) = Leaf k v
insert' (Leaf key val) (k, v)       
            | k < key = Node key val (Leaf k v) (EmptyTree)
            | k > key = Node key val (EmptyTree) (Leaf k v)
            | otherwise = Leaf k v

insert' (Node key val lt rt) (k, v)  
            | k < key = (Node key val (insert' lt (k,v)) rt)
            | k > key = (Node key val lt (insert' rt (k,v)))
            | otherwise = Node k v lt rt

-- Построение дерева из списка пар
treeFromList :: [(Integer, v)] -> TreeMap v
treeFromList [] = EmptyTree
treeFromList list = foldl insert' EmptyTree list

-- Построение списка пар из дерева
listFromTree :: TreeMap v -> [(Integer, v)]
listFromTree EmptyTree = []
listFromTree (Leaf key v) = [(key,v)]
listFromTree (Node key v lt rt) = 
                (listFromTree lt) ++ [(key,v)] ++ (listFromTree rt)

-- Поиск k-той порядковой статистики дерева 
kMean :: Integer -> TreeMap v -> (Integer, v)
kMean i EmptyTree = error "The tree is empty."
kMean i (Leaf key v)   | i == 0 = (key, v)
                       | otherwise = error "No such element in tree."

kMean i (Node key v (Leaf key' v') (Leaf key'' v''))    
                        | i == 0 = (key', v')
                        | i == 1 = (key, v)
                        | i == 2 = (key'', v'')
                        | otherwise = error "No such element."

kMean i (Node key v EmptyTree (Leaf key'' v''))         
                        | i == 0 = (key, v)
                        | i == 1 = (key'', v'')
                        | otherwise = error "No such element."

kMean i (Node key v (Leaf key' v') EmptyTree)           
                        | i == 0 = (key', v')
                        | i == 1 = (key, v)                                                        
                        | otherwise = error "No such element."

kMean i (Node key v lt rt)   
                        | (getMaxMeans lt) - (i + 1) >= 0 = kMean i lt
                        | (getMaxMeans lt) + 1 == (i + 1) = (key, v)
                        | otherwise = kMean (i - ((getMaxMeans lt) + 1)) rt 
    where 
        getMaxMeans EmptyTree = (-1)
        getMaxMeans (Leaf key' v') = 1
        getMaxMeans (Node key' v' (Leaf lKey lV) (Leaf rKey rV)) = 3
        getMaxMeans (Node key' v' lt' rt') = (getMaxMeans lt') + (getMaxMeans rt') + 1